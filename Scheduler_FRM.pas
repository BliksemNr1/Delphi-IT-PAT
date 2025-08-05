unit Scheduler_FRM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects, System.Rtti, FMX.Grid.Style,
  FMX.Grid, FMX.ScrollBox, FMX.StdCtrls, Data.DB, Data.Win.ADODB, System.UIConsts,
  System.Generics.Collections, FMX.TabControl, FMX.Layouts,
  System.Math; // *** ADDED: Required for Floor function ***

type
  TScheduledTaskInfo = record
    ScheduleID: Integer;
    TaskID: Integer;
    StaffID: Integer;
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
    rCancelTask: TRectangle;
    Label2: TLabel;
    TabControl1: TTabControl;
    Rectangle1: TRectangle;
    tiListView: TTabItem;
    tiTimelineView: TTabItem;
    Rectangle2: TRectangle;
    sbTimeline: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure rOptimizeClick(Sender: TObject);
    procedure rManualAssignClick(Sender: TObject);
    procedure UnscheduledGridCellClick(const Column: TColumn; const Row: Integer);
    procedure ScheduledGridCellClick(const Column: TColumn; const Row: Integer);
    procedure rMarkCompleteClick(Sender: TObject);
    procedure rCancelTaskClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    { Private declarations }
    FSelectedUnscheduledRow: Integer;
    FSelectedScheduledRow: Integer;
    FScheduledTasks: TArray<TScheduledTaskInfo>;
    // --- NEW: Variables for Drag-and-Drop ---
    FDraggingRect: TRectangle;
    FDragStartPoint: TPointF;
    FOriginalPosition: TPointF;
    // --- END NEW ---
    procedure LoadUnscheduledGrid;
    procedure LoadScheduledGrid;
    procedure DrawTimeline;
    // --- NEW: Event Handlers for Drag-and-Drop ---
    procedure TaskRectMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TaskRectMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure TaskRectMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    // --- END NEW ---
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

const
  // --- Constants for Timeline Drawing ---
  BayRowHeight = 60;
  HeaderHeight = 40;
  BayLabelWidth = 150;
  PixelsPerHour = 120;
  // --- End Constants ---

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
  FDraggingRect := nil; // Initialize dragging variable
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

    cmd.CommandText := 'UPDATE tblSchedule SET Status = ''Completed'', CompletedTime = :pNow WHERE ScheduleID = :pScheduleID';
    cmd.Parameters.ParamByName('pNow').Value := Now;
    cmd.Parameters.ParamByName('pScheduleID').Value := SelectedTaskInfo.ScheduleID;
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
begin
  ScheduledGrid.RowCount := 0;
  SetLength(FScheduledTasks, 0);
  try
    FmOpti.qrySchedule.Close;
    FmOpti.qrySchedule.SQL.Clear;
    FmOpti.qrySchedule.SQL.Add('SELECT s.ScheduleID, s.TaskID, s.StaffID, b.BayName, t.TaskName, s.StartTime, s.EndTime, t.Status');
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
      lEndTime := FmOpti.qrySchedule.FieldByName('EndTime').AsDateTime;

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

procedure TScheduler.rCancelTaskClick(Sender: TObject);
var
  SelectedTaskInfo: TScheduledTaskInfo;
  cmd: TADOCommand;
begin
  if FSelectedScheduledRow < 0 then
  begin
    ShowMessage('Please select a task from the "Scheduled Tasks" grid to cancel.');
    Exit;
  end;

  SelectedTaskInfo := FScheduledTasks[FSelectedScheduledRow];

  if MessageDlg('Are you sure you want to cancel this task? It will be removed from the schedule and the staff member will be released.',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrNo then
    Exit;

  cmd := TADOCommand.Create(nil);
  try
    cmd.Connection := FmOpti.ConOpti;
    FmOpti.ConOpti.BeginTrans;

    cmd.CommandText := 'UPDATE tblTasks SET Status = ''Canceled'' WHERE TaskID = :pTaskID';
    cmd.Parameters.ParamByName('pTaskID').Value := SelectedTaskInfo.TaskID;
    cmd.Execute;

    if SelectedTaskInfo.StaffID > 0 then
    begin
      cmd.CommandText := 'UPDATE tblStaff SET IsActive = False WHERE StaffID = :pStaffID';
      cmd.Parameters.ParamByName('pStaffID').Value := SelectedTaskInfo.StaffID;
      cmd.Execute;
    end;

    cmd.CommandText := 'DELETE FROM tblSchedule WHERE ScheduleID = :pScheduleID';
    cmd.Parameters.ParamByName('pScheduleID').Value := SelectedTaskInfo.ScheduleID;
    cmd.Execute;

    FmOpti.ConOpti.CommitTrans;
    ShowMessage('Task has been canceled and removed from the schedule.');

  except
    on E: Exception do
    begin
      FmOpti.ConOpti.RollbackTrans;
      ShowMessage('An error occurred while canceling the task: ' + E.Message);
    end;
  end;

  cmd.Free;
  LoadUnscheduledGrid;
  LoadScheduledGrid;
end;

procedure TScheduler.rManualAssignClick(Sender: TObject);
var
  lBayName: string;
  lTaskID, lBayID, lDuration: Integer;
  qryBayLookup, qryTaskLookup, cmd: TADOQuery;
  lEarliestStart, lEndTime: TDateTime;
  function GetLocalEarliestStartTime(const ABayID: Integer): TDateTime;
  var
    qry: TADOQuery;
  begin
    Result := Now;
    qry := TADOQuery.Create(nil);
    try
      qry.Connection := FmOpti.ConOpti;
      qry.SQL.Text := 'SELECT MAX(EndTime) AS LastEnd FROM tblSchedule WHERE BayID = :pBayID';
      qry.Parameters.ParamByName('pBayID').Value := ABayID;
      qry.Open;
      if not qry.FieldByName('LastEnd').IsNull then
        Result := qry.FieldByName('LastEnd').AsDateTime;
    finally
      qry.Free;
    end;
  end;
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
  qryTaskLookup := TADOQuery.Create(nil);
  cmd := TADOQuery.Create(nil);
  try
    qryBayLookup.Connection := FmOpti.ConOpti;
    qryTaskLookup.Connection := FmOpti.ConOpti;
    cmd.Connection := FmOpti.ConOpti;

    qryBayLookup.SQL.Text := 'SELECT BayID FROM tblBays WHERE BayName = :pBayName AND ManagerID = :pManagerID AND Status = ''Available''';
    qryBayLookup.Parameters.ParamByName('pBayName').Value := lBayName;
    qryBayLookup.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    qryBayLookup.Open;

    if qryBayLookup.IsEmpty then
    begin
      ShowMessage('Error: Bay "' + lBayName + '" not found or is not available.');
      Exit;
    end;
    lBayID := qryBayLookup.FieldByName('BayID').AsInteger;
    qryBayLookup.Close;

    qryTaskLookup.SQL.Text := 'SELECT Duration FROM tblTasks WHERE TaskID = :pTaskID';
    qryTaskLookup.Parameters.ParamByName('pTaskID').Value := lTaskID;
    qryTaskLookup.Open;
    if qryTaskLookup.IsEmpty then
    begin
      ShowMessage('Error: Could not find the details for the selected task.');
      Exit;
    end;
    lDuration := qryTaskLookup.FieldByName('Duration').AsInteger;
    qryTaskLookup.Close;

    lEarliestStart := GetLocalEarliestStartTime(lBayID);
    lEndTime := IncMinute(lEarliestStart, lDuration);

    FmOpti.ConOpti.BeginTrans;

    cmd.SQL.Text := 'INSERT INTO tblSchedule (TaskID, BayID, StartTime, EndTime, ManagerID) VALUES (:pTaskID, :pBayID, :pStartTime, :pEndTime, :pManagerID)';
    cmd.Parameters.ParamByName('pTaskID').Value := lTaskID;
    cmd.Parameters.ParamByName('pBayID').Value := lBayID;
    cmd.Parameters.ParamByName('pStartTime').Value := lEarliestStart;
    cmd.Parameters.ParamByName('pEndTime').Value := lEndTime;
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
  qryTaskLookup.Free;
  cmd.Free;
  LoadUnscheduledGrid;
  LoadScheduledGrid;
end;

procedure TScheduler.rOptimizeClick(Sender: TObject);
var
  CursorService: IFMXCursorService;
  bScheduledSomething: Boolean;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, IInterface(CursorService)) then
    CursorService.SetCursor(crHourGlass);
  try
    try
      bScheduledSomething := FmOpti.OptimizeSchedule(CurrentManagerID);

      if bScheduledSomething then
        ShowMessage('Optimization process completed successfully!')
      else
        ShowMessage('No pending tasks could be scheduled with the available bays.');

    except
      on E: Exception do
      begin
        ShowMessage('An error occurred during optimization: ' + E.Message);
      end;
    end;
  finally
    if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, IInterface(CursorService)) then
      CursorService.SetCursor(crDefault);
  end;

  LoadUnscheduledGrid;
  LoadScheduledGrid;
end;

procedure TScheduler.TabControl1Change(Sender: TObject);
begin
  if TabControl1.ActiveTab = tiTimelineView then
  begin
    DrawTimeline;
  end;
end;

procedure TScheduler.DrawTimeline;
var
  qryBays: TADOQuery;
  qrySchedule: TADOQuery;
  BayDict: TDictionary<Integer, Integer>;
  i: Integer;
  BayLabel: TLabel;
  HourLabel: TLabel;
  TaskRect: TRectangle;
  TaskLabel: TLabel;
  lStartTime, lEndTime: TDateTime;
  lTaskName, lPriority: string;
  lBayID, lScheduleID: Integer;
  RowIndex: Integer;
  StartX, TaskWidth: Single;
  TaskColor: TAlphaColor;
  TotalContentHeight, TotalContentWidth: Single;
begin
  while sbTimeline.ControlsCount > 0 do
    sbTimeline.Controls[0].Free;

  BayDict := TDictionary<Integer, Integer>.Create;
  qryBays := TADOQuery.Create(nil);
  qrySchedule := TADOQuery.Create(nil);
  try
    qryBays.Connection := FmOpti.ConOpti;
    qryBays.SQL.Text := 'SELECT BayID, BayName FROM tblBays WHERE ManagerID = :pManagerID ORDER BY BayName';
    qryBays.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    qryBays.Open;

    i := 0;
    while not qryBays.Eof do
    begin
      BayLabel := TLabel.Create(sbTimeline);
      BayLabel.Parent := sbTimeline;
      BayLabel.Position.X := 10;
      BayLabel.Position.Y := HeaderHeight + (i * BayRowHeight);
      BayLabel.Width := BayLabelWidth - 20;
      BayLabel.Height := BayRowHeight;
      BayLabel.Text := qryBays.FieldByName('BayName').AsString;
      BayLabel.TextAlign := TTextAlign.Center;
      BayLabel.Font.Size := 14;
      BayLabel.FontColor := TAlphaColors.White;
      BayLabel.Tag := qryBays.FieldByName('BayID').AsInteger;

      BayDict.Add(qryBays.FieldByName('BayID').AsInteger, i);
      Inc(i);
      qryBays.Next;
    end;

    for i := 0 to 23 do
    begin
      HourLabel := TLabel.Create(sbTimeline);
      HourLabel.Parent := sbTimeline;
      HourLabel.Position.X := BayLabelWidth + (i * PixelsPerHour);
      HourLabel.Position.Y := 10;
      HourLabel.Width := PixelsPerHour;
      HourLabel.Height := HeaderHeight - 10;
      HourLabel.Text := Format('%.2d:00', [i]);
      HourLabel.FontColor := TAlphaColors.Lightgray;
    end;

    qrySchedule.Connection := FmOpti.ConOpti;
    qrySchedule.SQL.Text := 'SELECT s.ScheduleID, s.BayID, t.TaskName, s.StartTime, s.EndTime, t.Priority ' +
                           'FROM tblSchedule s INNER JOIN tblTasks t ON s.TaskID = t.TaskID ' +
                           'WHERE t.ManagerID = :pManagerID AND t.Status IN (''Scheduled'', ''In Progress'') ' +
                           'AND s.StartTime >= :pStartDate AND s.StartTime < :pEndDate';
    qrySchedule.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    qrySchedule.Parameters.ParamByName('pStartDate').Value := Trunc(Now);
    qrySchedule.Parameters.ParamByName('pEndDate').Value := Trunc(Now) + 1;
    qrySchedule.Open;

    while not qrySchedule.Eof do
    begin
      lBayID := qrySchedule.FieldByName('BayID').AsInteger;
      if BayDict.TryGetValue(lBayID, RowIndex) then
      begin
        // *** ROBUSTNESS FIX: Check for NULL time values before reading them ***
        if (not qrySchedule.FieldByName('StartTime').IsNull) and (not qrySchedule.FieldByName('EndTime').IsNull) then
        begin
          lScheduleID := qrySchedule.FieldByName('ScheduleID').AsInteger;
          lStartTime := qrySchedule.FieldByName('StartTime').AsDateTime;
          lEndTime := qrySchedule.FieldByName('EndTime').AsDateTime;
          lTaskName := qrySchedule.FieldByName('TaskName').AsString;
          lPriority := qrySchedule.FieldByName('Priority').AsString;

          StartX := BayLabelWidth + (HourOf(lStartTime) * PixelsPerHour) + (MinuteOf(lStartTime) * PixelsPerHour / 60);
          TaskWidth := MinutesBetween(lEndTime, lStartTime) * PixelsPerHour / 60;

          if lPriority = 'High' then
            TaskColor := TAlphaColor($FFD9534F)
          else if lPriority = 'Medium' then
            TaskColor := TAlphaColor($FFF0AD4E)
          else
            TaskColor := TAlphaColor($FF5CB85C);

          TaskRect := TRectangle.Create(sbTimeline);
          TaskRect.Parent := sbTimeline;
          TaskRect.Tag := lScheduleID;
          TaskRect.Position.X := StartX;
          TaskRect.Position.Y := HeaderHeight + (RowIndex * BayRowHeight) + 5;
          TaskRect.Width := TaskWidth;
          TaskRect.Height := BayRowHeight - 10;
          TaskRect.Fill.Color := TaskColor;
          TaskRect.Stroke.Kind := TBrushKind.None;
          TaskRect.XRadius := 5;
          TaskRect.YRadius := 5;
          TaskRect.OnMouseDown := TaskRectMouseDown;
          TaskRect.OnMouseMove := TaskRectMouseMove;
          TaskRect.OnMouseUp := TaskRectMouseUp;

          TaskLabel := TLabel.Create(TaskRect);
          TaskLabel.Parent := TaskRect;
          TaskLabel.Align := TAlignLayout.Client;
          TaskLabel.Text := ' ' + lTaskName;
          TaskLabel.HitTest := False;
          TaskLabel.TextAlign := TTextAlign.Center;
          TaskLabel.FontColor := TAlphaColors.Black;
          TaskLabel.Font.Size := 12;
        end;
      end;
      qrySchedule.Next;
    end;

    TotalContentHeight := HeaderHeight + (qryBays.RecordCount * BayRowHeight) + 20;
    TotalContentWidth := BayLabelWidth + (24 * PixelsPerHour) + 20;
    // *** CORRECTED: Assigning to ContentSize property correctly ***
    sbTimeline.Content.BoundsRect := TRectF.Create(0, 0, TotalContentWidth, TotalContentHeight);


  finally
    BayDict.Free;
    qryBays.Free;
    qrySchedule.Free;
  end;
end;

procedure TScheduler.TaskRectMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then
  begin
    FDraggingRect := Sender as TRectangle;
    FDraggingRect.BringToFront;
    FOriginalPosition := FDraggingRect.Position.Point;
    FDragStartPoint := TPointF.Create(X, Y);
  end;
end;

procedure TScheduler.TaskRectMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if (FDraggingRect <> nil) and (ssLeft in Shift) then
  begin
    FDraggingRect.Position.X := FDraggingRect.Position.X + X - FDragStartPoint.X;
    FDraggingRect.Position.Y := FDraggingRect.Position.Y + Y - FDragStartPoint.Y;
  end;
end;

procedure TScheduler.TaskRectMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  DropPoint: TPointF;
  TargetBayRow, TargetBayID, ScheduleID, Duration: Integer;
  NewStartTime, NewEndTime: TDateTime;
  qry: TADOQuery;
  IsConflict: Boolean;
begin
  if FDraggingRect = nil then
    Exit;

  try
    DropPoint := FDraggingRect.Position.Point;
    ScheduleID := FDraggingRect.Tag;

    TargetBayRow := Floor((DropPoint.Y - HeaderHeight) / BayRowHeight);
    if (TargetBayRow < 0) then
    begin
      ShowMessage('Invalid drop location.');
      FDraggingRect.Position.Point := FOriginalPosition;
      Exit;
    end;

    TargetBayID := -1;
    for var i := 0 to sbTimeline.ControlsCount - 1 do
    begin
      if (sbTimeline.Controls[i] is TLabel) and (TObject(sbTimeline.Controls[i]).ClassNameIs('TLabel')) then
      begin
        var Lbl := TLabel(sbTimeline.Controls[i]);
        if Lbl.Tag > 0 then // Bay labels have a Tag > 0, hour labels do not
        begin
          if Floor((Lbl.Position.Y - HeaderHeight) / BayRowHeight) = TargetBayRow then
          begin
            TargetBayID := Lbl.Tag;
            Break;
          end;
        end;
      end;
    end;

    if TargetBayID = -1 then
    begin
      ShowMessage('Could not determine target bay.');
      FDraggingRect.Position.Point := FOriginalPosition;
      Exit;
    end;

    var TotalMinutes: Double;
    TotalMinutes := (DropPoint.X - BayLabelWidth) / (PixelsPerHour / 60);
    NewStartTime := Trunc(Now) + (TotalMinutes / (24 * 60));

    qry := TADOQuery.Create(nil);
    try
      qry.Connection := FmOpti.ConOpti;
      qry.SQL.Text := 'SELECT t.Duration FROM tblSchedule s INNER JOIN tblTasks t ON s.TaskID = t.TaskID WHERE s.ScheduleID = :pScheduleID';
      qry.Parameters.ParamByName('pScheduleID').Value := ScheduleID;
      qry.Open;
      if qry.IsEmpty then
      begin
        ShowMessage('Error: Could not find task details.');
        FDraggingRect.Position.Point := FOriginalPosition;
        Exit;
      end;
      Duration := qry.FieldByName('Duration').AsInteger;
    finally
      qry.Free;
    end;

    NewEndTime := IncMinute(NewStartTime, Duration);

    IsConflict := False;
    qry := TADOQuery.Create(nil);
    try
      qry.Connection := FmOpti.ConOpti;
      qry.SQL.Text := 'SELECT COUNT(*) AS ConflictCount FROM tblSchedule ' +
                     'WHERE BayID = :pBayID AND ScheduleID <> :pScheduleID ' +
                     'AND EndTime > :pNewStart AND StartTime < :pNewEnd';
      qry.Parameters.ParamByName('pBayID').Value := TargetBayID;
      qry.Parameters.ParamByName('pScheduleID').Value := ScheduleID;
      qry.Parameters.ParamByName('pNewStart').Value := NewStartTime;
      qry.Parameters.ParamByName('pNewEnd').Value := NewEndTime;
      qry.Open;
      if qry.FieldByName('ConflictCount').AsInteger > 0 then
      begin
        IsConflict := True;
      end;
    finally
      qry.Free;
    end;

    if IsConflict then
    begin
      ShowMessage('This move creates a schedule conflict. Reverting.');
      FDraggingRect.Position.Point := FOriginalPosition;
    end
    else
    begin
      var cmd: TADOCommand;
      cmd := TADOCommand.Create(nil);
      try
        cmd.Connection := FmOpti.ConOpti;
        cmd.CommandText := 'UPDATE tblSchedule SET BayID = :pBayID, StartTime = :pStart, EndTime = :pEnd WHERE ScheduleID = :pScheduleID';
        cmd.Parameters.ParamByName('pBayID').Value := TargetBayID;
        cmd.Parameters.ParamByName('pStart').Value := NewStartTime;
        cmd.Parameters.ParamByName('pEnd').Value := NewEndTime;
        cmd.Parameters.ParamByName('pScheduleID').Value := ScheduleID;
        cmd.Execute;
        ShowMessage('Task rescheduled successfully.');
        LoadScheduledGrid;
        DrawTimeline;
      except
        on E: Exception do
        begin
          ShowMessage('Database error on update: ' + E.Message);
          FDraggingRect.Position.Point := FOriginalPosition;
        end;
      end;
      cmd.Free;
    end;

  finally
    FDraggingRect := nil;
  end;
end;

end.

