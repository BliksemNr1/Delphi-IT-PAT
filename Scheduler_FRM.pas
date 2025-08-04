unit Scheduler_FRM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects, System.Rtti, FMX.Grid.Style,
  FMX.Grid, FMX.ScrollBox, FMX.StdCtrls, Data.DB, Data.Win.ADODB, System.UIConsts,
  System.Generics.Collections;

type
  TScheduledTaskInfo = record
    ScheduleID: Integer;
    TaskID: Integer;
    StaffID: Integer;
  end;

  TBayInfo = record
    BayID: Integer;
    BayType: string;
    NextAvailableTime: TDateTime;
  end;

  TScheduler = class(TForm)
    Rectangle21: TRectangle;
    UnscheduledGrid: TStringGrid;
    colTaskName: TStringColumn;
    colBayType: TStringColumn;
    colDuration: TStringColumn;
    colPriority: TStringColumn;
    colRequiredBay: TStringColumn;
    Rectangle17: TRectangle;
    ScheduledGrid: TStringGrid;
    colBayName: TStringColumn;
    colTaskenameScheduled: TStringColumn;
    colStartTime: TStringColumn;
    colEndTime: TStringColumn;
    ColStatus: TStringColumn;
    labels: TLabel;
    Label1: TLabel;
    rOptimize: TRectangle;
    Label4: TLabel;
    rManualAssign: TRectangle;
    Label6: TLabel;
    rMarkComplete: TRectangle;
    Label5: TLabel;
    rLogoImage: TRectangle;
    rLogo: TRectangle;
    rDashBoardMenubtn: TRectangle;
    DashBoardbtnICON: TRectangle;
    lblDashBoardBtn: TEdit;
    rTaskManagerMenuBtn: TRectangle;
    TaskManagerIcon: TRectangle;
    lblTaskManagerBtn: TEdit;
    rBayManagerMenuBtn: TRectangle;
    BayManagerIcon: TRectangle;
    lblBayManagerbtn: TEdit;
    rSchedulerMenuBtn: TRectangle;
    SchedulerIcon: TRectangle;
    lblSchedulerBtn: TEdit;
    rReportsMenuBtn: TRectangle;
    ReportsIcon: TRectangle;
    lblReportsbtn: TEdit;
    rStaffManagerMenubtn: TRectangle;
    StaffManagerIcon: TRectangle;
    lblStaffManagerBtn: TEdit;
    rStaffSchedulerMenuBtn: TRectangle;
    staffSchedulerICON: TRectangle;
    lblStaffSchedulerbtn: TEdit;
    LogoutIcon: TRectangle;
    LogoutBtn: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure rOptimizeClick(Sender: TObject);
    procedure rManualAssignClick(Sender: TObject);
    procedure UnscheduledGridCellClick(const Column: TColumn; const Row: Integer);
    procedure ScheduledGridCellClick(const Column: TColumn; const Row: Integer);
    procedure rMarkCompleteClick(Sender: TObject);
  private
    { Private declarations }
    FSelectedUnscheduledRow: Integer;
    FSelectedScheduledRow: Integer;
    FScheduledTasks: TArray<TScheduledTaskInfo>;
    procedure LoadUnscheduledGrid;
    procedure LoadScheduledGrid;
    function GetEarliestStartTimeForBay(const ABayID: Integer): TDateTime;
  public
    { Public declarations }
  published
    procedure GridApplyStyleLookup(Sender: TObject);
    procedure GridDrawColumnHeader(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF);
    procedure GridDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
  end;

var
  Scheduler: TScheduler;

implementation

{$R *.fmx}

uses System.DateUtils, dmOpti, Login_Frm, FMX.DialogService, FMX.Platform;

procedure TScheduler.FormCreate(Sender: TObject);
begin
  UnscheduledGrid.DefaultDrawing := False;
  ScheduledGrid.DefaultDrawing := False;
  UnscheduledGrid.Options := UnscheduledGrid.Options - [TGridOption.Editing];
  ScheduledGrid.Options := ScheduledGrid.Options - [TGridOption.Editing];
  UnscheduledGrid.OnApplyStyleLookup := GridApplyStyleLookup;
  UnscheduledGrid.OnDrawColumnHeader := GridDrawColumnHeader;
  UnscheduledGrid.OnDrawColumnCell := GridDrawColumnCell;
  ScheduledGrid.OnApplyStyleLookup := GridApplyStyleLookup;
  ScheduledGrid.OnDrawColumnHeader := GridDrawColumnHeader;
  ScheduledGrid.OnDrawColumnCell := GridDrawColumnCell;
  UnscheduledGrid.OnCellClick := UnscheduledGridCellClick;
  ScheduledGrid.OnCellClick := ScheduledGridCellClick;
  FSelectedUnscheduledRow := -1;
  FSelectedScheduledRow := -1;
  LoadUnscheduledGrid;
  LoadScheduledGrid;
end;

procedure TScheduler.UnscheduledGridCellClick(const Column: TColumn; const Row: Integer);
begin
  FSelectedUnscheduledRow := Row;
  ScheduledGrid.Selected := -1;
  FSelectedScheduledRow := -1;
end;

procedure TScheduler.ScheduledGridCellClick(const Column: TColumn; const Row: Integer);
begin
  FSelectedScheduledRow := Row;
  UnscheduledGrid.Selected := -1;
  FSelectedUnscheduledRow := -1;
end;

procedure TScheduler.rMarkCompleteClick(Sender: TObject);
var
  SelectedTaskInfo: TScheduledTaskInfo;
  cmd: TADOCommand;
begin
  if FSelectedScheduledRow < 0 then
  begin
    ShowMessage('Please select a task from the "Scheduled Tasks" grid to mark as complete.');
    Exit;
  end;

  SelectedTaskInfo := FScheduledTasks[FSelectedScheduledRow];

  if MessageDlg('Are you sure you want to mark this task as "Completed"? This will release the assigned staff member.',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrNo then
    Exit;

  cmd := TADOCommand.Create(nil);
  try
    cmd.Connection := FmOpti.ConOpti;
    FmOpti.ConOpti.BeginTrans;

    cmd.CommandText := 'UPDATE tblTasks SET Status = ''Completed'' WHERE TaskID = :pTaskID';
    cmd.Parameters.ParamByName('pTaskID').Value := SelectedTaskInfo.TaskID;
    cmd.Execute;

    if SelectedTaskInfo.StaffID > 0 then
    begin
      cmd.CommandText := 'UPDATE tblStaff SET IsActive = False WHERE StaffID = :pStaffID';
      cmd.Parameters.ParamByName('pStaffID').Value := SelectedTaskInfo.StaffID;
      cmd.Execute;
    end;

    FmOpti.ConOpti.CommitTrans;
    ShowMessage('Task marked as complete and staff member has been released.');

  except
    on E: Exception do
    begin
      FmOpti.ConOpti.RollbackTrans;
      ShowMessage('An error occurred: ' + E.Message);
    end;
  end;

  cmd.Free;
  LoadUnscheduledGrid;
  LoadScheduledGrid;
end;

procedure TScheduler.GridApplyStyleLookup(Sender: TObject);
var
  LBackground: TFmxObject;
  Grid: TStringGrid;
begin
  Grid := Sender as TStringGrid;
  LBackground := Grid.FindStyleResource('background');
  if (LBackground is TControl) then
    (LBackground as TControl).Visible := False;
end;

procedure TScheduler.GridDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
  const Column: TColumn; const Bounds: TRectF; const Row: Integer;
  const Value: TValue; const State: TGridDrawStates);
var
  LBackgroundColor: TAlphaColor;
begin
  if Odd(Row) then
    LBackgroundColor := TAlphaColor($FF2A343B)
  else
    LBackgroundColor := TAlphaColor($FF1F272C);
  Canvas.Fill.Color := LBackgroundColor;
  Canvas.FillRect(Bounds, 0, 0, [], 1.0);
  Canvas.Fill.Color := TAlphaColor($FFF8F9FA);
  Canvas.FillText(Bounds, Value.ToString, False, 1.0, [], TTextAlign.Leading, TTextAlign.Center);
end;

procedure TScheduler.GridDrawColumnHeader(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF);
begin
  Canvas.Fill.Color := TAlphaColor($FF1F272C);
  Canvas.FillRect(Bounds, 0, 0, [], 1.0);
  Canvas.Fill.Color := TAlphaColor($FFF8F9FA);
  Canvas.FillText(Bounds, Column.Header, False, 1.0, [], TTextAlign.Center, TTextAlign.Center);
end;

procedure TScheduler.LoadUnscheduledGrid;
var
  iRow: Integer;
begin
  UnscheduledGrid.RowCount := 0;
  try
    FmOpti.qryTasks.Close;
    FmOpti.qryTasks.SQL.Clear;
    FmOpti.qryTasks.SQL.Add('SELECT TaskID, TaskName, BayType, Duration, Priority FROM tblTasks');
    FmOpti.qryTasks.SQL.Add('WHERE (Status IS NULL OR Status = ''Pending'')');
    FmOpti.qryTasks.SQL.Add('AND ManagerID = :pManagerID');
    FmOpti.qryTasks.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    FmOpti.qryTasks.Open;

    iRow := 0;
    FmOpti.qryTasks.First;
    while not FmOpti.qryTasks.Eof do
    begin
      UnscheduledGrid.RowCount := iRow + 1;
      UnscheduledGrid.Cells[colTaskName.Index, iRow] := FmOpti.qryTasks.FieldByName('TaskName').AsString;
      UnscheduledGrid.Cells[colBayType.Index, iRow] := FmOpti.qryTasks.FieldByName('BayType').AsString;
      UnscheduledGrid.Cells[colDuration.Index, iRow] := FmOpti.qryTasks.FieldByName('Duration').AsString + ' min';
      UnscheduledGrid.Cells[colPriority.Index, iRow] := FmOpti.qryTasks.FieldByName('Priority').AsString;
      UnscheduledGrid.Cells[colRequiredBay.Index, iRow] := FmOpti.qryTasks.FieldByName('TaskID').AsString;
      Inc(iRow);
      FmOpti.qryTasks.Next;
    end;
  except
    on E: Exception do
      ShowMessage('Error in LoadUnscheduledGrid: ' + E.Message);
  end;
  FmOpti.qryTasks.Close;
  FSelectedUnscheduledRow := -1;
end;

procedure TScheduler.LoadScheduledGrid;
var
  iRow: Integer;
  lStartTime, lEndTime: TDateTime;
  lDuration: Integer;
begin
  ScheduledGrid.RowCount := 0;
  SetLength(FScheduledTasks, 0);
  try
    FmOpti.qrySchedule.Close;
    FmOpti.qrySchedule.SQL.Clear;
    FmOpti.qrySchedule.SQL.Add('SELECT s.ScheduleID, s.TaskID, s.StaffID, b.BayName, t.TaskName, s.StartTime, t.Duration, t.Status');
    FmOpti.qrySchedule.SQL.Add('FROM ((tblSchedule s');
    FmOpti.qrySchedule.SQL.Add('INNER JOIN tblTasks t ON s.TaskID = t.TaskID)');
    FmOpti.qrySchedule.SQL.Add('INNER JOIN tblBays b ON s.BayID = b.BayID)');
    FmOpti.qrySchedule.SQL.Add('WHERE t.ManagerID = :pManagerID AND (t.Status = ''Scheduled'' OR t.Status = ''In Progress'')');
    FmOpti.qrySchedule.SQL.Add('ORDER BY s.StartTime');
    FmOpti.qrySchedule.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    FmOpti.qrySchedule.Open;

    iRow := 0;
    FmOpti.qrySchedule.First;
    while not FmOpti.qrySchedule.Eof do
    begin
      ScheduledGrid.RowCount := iRow + 1;
      SetLength(FScheduledTasks, iRow + 1);
      FScheduledTasks[iRow].ScheduleID := FmOpti.qrySchedule.FieldByName('ScheduleID').AsInteger;
      FScheduledTasks[iRow].TaskID := FmOpti.qrySchedule.FieldByName('TaskID').AsInteger;
      FScheduledTasks[iRow].StaffID := FmOpti.qrySchedule.FieldByName('StaffID').AsInteger;

      lStartTime := FmOpti.qrySchedule.FieldByName('StartTime').AsDateTime;
      lDuration := FmOpti.qrySchedule.FieldByName('Duration').AsInteger;
      lEndTime := IncMinute(lStartTime, lDuration);
      ScheduledGrid.Cells[colBayName.Index, iRow] := FmOpti.qrySchedule.FieldByName('BayName').AsString;
      ScheduledGrid.Cells[colTaskenameScheduled.Index, iRow] := FmOpti.qrySchedule.FieldByName('TaskName').AsString;
      ScheduledGrid.Cells[colStartTime.Index, iRow] := FormatDateTime('yyyy-mm-dd hh:nn', lStartTime);
      ScheduledGrid.Cells[colEndTime.Index, iRow] := FormatDateTime('yyyy-mm-dd hh:nn', lEndTime);
      ScheduledGrid.Cells[ColStatus.Index, iRow] := FmOpti.qrySchedule.FieldByName('Status').AsString;
      Inc(iRow);
      FmOpti.qrySchedule.Next;
    end;
  except
    on E: Exception do
      ShowMessage('Error in LoadScheduledGrid: ' + E.Message);
  end;
  FmOpti.qrySchedule.Close;
  FSelectedScheduledRow := -1;
end;

function TScheduler.GetEarliestStartTimeForBay(const ABayID: Integer): TDateTime;
var
  qryLastTask: TADOQuery;
  lLastTaskStart: TDateTime;
  lLastTaskDuration: Integer;
begin
  Result := Now;
  qryLastTask := TADOQuery.Create(nil);
  try
    qryLastTask.Connection := FmOpti.ConOpti;
    qryLastTask.SQL.Text := 'SELECT TOP 1 s.StartTime, t.Duration FROM tblSchedule s ' +
                          'INNER JOIN tblTasks t ON s.TaskID = t.TaskID ' +
                          'WHERE s.BayID = :pBayID ORDER BY s.StartTime DESC';
    qryLastTask.Parameters.ParamByName('pBayID').Value := ABayID;
    qryLastTask.Open;

    if not qryLastTask.IsEmpty then
    begin
      lLastTaskStart := qryLastTask.FieldByName('StartTime').AsDateTime;
      lLastTaskDuration := qryLastTask.FieldByName('Duration').AsInteger;
      Result := IncMinute(lLastTaskStart, lLastTaskDuration);
    end;
  finally
    qryLastTask.Free;
  end;
end;

procedure TScheduler.rManualAssignClick(Sender: TObject);
var
  lBayName: string;
  lTaskID, lBayID: Integer;
  qryBayLookup, cmd: TADOQuery;
  lEarliestStart: TDateTime;
begin
  if FSelectedUnscheduledRow < 0 then
  begin
    ShowMessage('Please select a task from the "Unscheduled Tasks" grid first.');
    Exit;
  end;

  if not InputQuery('Manual Assignment', 'Enter the name of the bay:', lBayName) or lBayName.IsEmpty then
    Exit;

  lTaskID := StrToInt(UnscheduledGrid.Cells[colRequiredBay.Index, FSelectedUnscheduledRow]);

  qryBayLookup := TADOQuery.Create(nil);
  cmd := TADOQuery.Create(nil);
  try
    qryBayLookup.Connection := FmOpti.ConOpti;
    cmd.Connection := FmOpti.ConOpti;

    qryBayLookup.SQL.Text := 'SELECT BayID FROM tblBays WHERE BayName = :pBayName AND ManagerID = :pManagerID AND Status = ''Available''';
    qryBayLookup.Parameters.ParamByName('pBayName').Value := lBayName;
    qryBayLookup.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    qryBayLookup.Open;

    if qryBayLookup.IsEmpty then
    begin
      ShowMessage('Error: Bay "' + lBayName + '" not found or is not available (e.g., under maintenance).');
      Exit;
    end;
    lBayID := qryBayLookup.FieldByName('BayID').AsInteger;
    qryBayLookup.Close;

    lEarliestStart := GetEarliestStartTimeForBay(lBayID);

    FmOpti.ConOpti.BeginTrans;
    cmd.SQL.Text := 'INSERT INTO tblSchedule (TaskID, BayID, StartTime, ManagerID) VALUES (:pTaskID, :pBayID, :pStartTime, :pManagerID)';
    cmd.Parameters.ParamByName('pTaskID').Value := lTaskID;
    cmd.Parameters.ParamByName('pBayID').Value := lBayID;
    cmd.Parameters.ParamByName('pStartTime').Value := lEarliestStart;
    cmd.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    cmd.ExecSQL;

    cmd.SQL.Text := 'UPDATE tblTasks SET Status = ''Scheduled'' WHERE TaskID = :pTaskID';
    cmd.Parameters.ParamByName('pTaskID').Value := lTaskID;
    cmd.ExecSQL;
    FmOpti.ConOpti.CommitTrans;

    ShowMessage('Task successfully assigned to ' + lBayName);

  except
    on E: Exception do
    begin
      FmOpti.ConOpti.RollbackTrans;
      ShowMessage('An error occurred during manual assignment: ' + E.Message);
    end;
  end;

  qryBayLookup.Free;
  cmd.Free;
  LoadUnscheduledGrid;
  LoadScheduledGrid;
end;

procedure TScheduler.rOptimizeClick(Sender: TObject);
var
  qryUnscheduled, qryAvailableBays, cmd: TADOQuery;
  lAvailableBays: TList<TBayInfo>;
  lBayInfo: TBayInfo;
  lTaskID, lDuration, i: Integer;
  lRequiredBayType: string;
  lBestBayID, lBestBayIndex: Integer;
  lBestStartTime: TDateTime;
  bScheduledSomething: Boolean;
  CursorService: IFMXCursorService;
begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, IInterface(CursorService)) then
    CursorService.SetCursor(crHourGlass);

  try
    bScheduledSomething := False;
    lAvailableBays := TList<TBayInfo>.Create;
    qryUnscheduled := TADOQuery.Create(nil);
    qryAvailableBays := TADOQuery.Create(nil);
    cmd := TADOQuery.Create(nil);
    try
      qryUnscheduled.Connection := FmOpti.ConOpti;
      qryAvailableBays.Connection := FmOpti.ConOpti;
      cmd.Connection := FmOpti.ConOpti;

      qryAvailableBays.SQL.Text := 'SELECT BayID, BayType FROM tblBays WHERE Status = ''Available'' AND ManagerID = :pManagerID';
      qryAvailableBays.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
      qryAvailableBays.Open;
      while not qryAvailableBays.Eof do
      begin
        lBayInfo.BayID := qryAvailableBays.FieldByName('BayID').AsInteger;
        lBayInfo.BayType := qryAvailableBays.FieldByName('BayType').AsString;
        lBayInfo.NextAvailableTime := GetEarliestStartTimeForBay(lBayInfo.BayID);
        lAvailableBays.Add(lBayInfo);
        qryAvailableBays.Next;
      end;
      qryAvailableBays.Close;

      if lAvailableBays.Count = 0 then
      begin
        ShowMessage('No available bays to schedule tasks on.');
        Exit;
      end;

      FmOpti.ConOpti.BeginTrans;

      qryUnscheduled.SQL.Text := 'SELECT TaskID, Duration, BayType, Priority FROM tblTasks ' +
                               'WHERE (Status IS NULL OR Status = ''Pending'') AND ManagerID = :pManagerID ' +
                               'ORDER BY IIF(Priority="High", 1, IIF(Priority="Medium", 2, 3))';
      qryUnscheduled.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
      qryUnscheduled.Open;

      while not qryUnscheduled.Eof do
      begin
        lTaskID := qryUnscheduled.FieldByName('TaskID').AsInteger;
        lDuration := qryUnscheduled.FieldByName('Duration').AsInteger;
        lRequiredBayType := qryUnscheduled.FieldByName('BayType').AsString;
        lBestBayID := -1;
        lBestBayIndex := -1;
        lBestStartTime := EncodeDate(2099, 1, 1);

        for i := 0 to lAvailableBays.Count - 1 do
        begin
          lBayInfo := lAvailableBays[i];
          if (lBayInfo.BayType = lRequiredBayType) and (lBayInfo.NextAvailableTime < lBestStartTime) then
          begin
            lBestStartTime := lBayInfo.NextAvailableTime;
            lBestBayID := lBayInfo.BayID;
            lBestBayIndex := i;
          end;
        end;

        if lBestBayID <> -1 then
        begin
          cmd.SQL.Text := 'INSERT INTO tblSchedule (TaskID, BayID, StartTime, ManagerID) VALUES (:pTaskID, :pBayID, :pStartTime, :pManagerID)';
          cmd.Parameters.ParamByName('pTaskID').Value := lTaskID;
          cmd.Parameters.ParamByName('pBayID').Value := lBestBayID;
          cmd.Parameters.ParamByName('pStartTime').Value := lBestStartTime;
          cmd.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;

          cmd.ExecSQL;

          cmd.SQL.Text := 'UPDATE tblTasks SET Status = ''Scheduled'' WHERE TaskID = :pTaskID';
          cmd.Parameters.ParamByName('pTaskID').Value := lTaskID;
          cmd.ExecSQL;

          lBayInfo := lAvailableBays[lBestBayIndex];
          lBayInfo.NextAvailableTime := IncMinute(lBestStartTime, lDuration);
          lAvailableBays[lBestBayIndex] := lBayInfo;

          bScheduledSomething := True;
        end;
        qryUnscheduled.Next;
      end;

      FmOpti.ConOpti.CommitTrans;
      if bScheduledSomething then
        ShowMessage('Optimization process completed successfully!')
      else
        ShowMessage('No pending tasks could be scheduled with the available bays.');

    except
      on E: Exception do
      begin
        FmOpti.ConOpti.RollbackTrans;
        ShowMessage('An error occurred during optimization: ' + E.Message);
      end;
    end;

    lAvailableBays.Free;
    qryUnscheduled.Free;
    qryAvailableBays.Free;
    cmd.Free;

  finally

     if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, IInterface(CursorService)) then
    CursorService.SetCursor(crDefault);
  end;

  LoadUnscheduledGrid;
  LoadScheduledGrid;
end;

end.

