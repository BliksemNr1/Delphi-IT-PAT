unit StaffScheduler_FRM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects, System.Rtti, FMX.Grid.Style,
  FMX.StdCtrls, FMX.Grid, FMX.ScrollBox, FMX.ListBox,
  Data.DB, Data.Win.ADODB;

type
  TStaffScheduler = class(TForm)
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
    Rectangle17: TRectangle;
    Rectangle21: TRectangle;
    ScheduledTasksGrid: TStringGrid;
    colTaskName: TStringColumn;
    colBay: TStringColumn;
    colStartTime: TStringColumn;
    StaffAssignGrid: TStringGrid;
    colStaffMember: TStringColumn;
    colTaskNameStaff: TStringColumn;
    colBayStaff: TStringColumn;
    labels: TLabel;
    Label1: TLabel;
    colStartTimeStaff: TStringColumn;
    Rectangle20: TRectangle;
    cbSelectStaff: TComboBox;
    StyleBook1: TStyleBook;
    rAssignStaff: TRectangle;
    Label4: TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure rAssignStaffClick(Sender: TObject);
    procedure ScheduledTasksGridCellClick(const Column: TColumn; const Row: Integer);
    procedure GridDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure GridDrawColumnHeader(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF);
    procedure GridApplyStyleLookup(Sender: TObject);
    procedure cbSelectStaffChange(Sender: TObject);
  private
    { Private declarations }
    FSelectedScheduleID: Integer;
    FStaffIDs: TStringList;
    FScheduleIDs: TArray<Integer>;
    procedure LoadData;
    procedure LoadScheduledTasks;
    procedure LoadStaffAssignments;
    procedure UpdateAvailableStaffForTask;
  public
    { Public declarations }
  end;

var
  StaffScheduler: TStaffScheduler;

implementation

{$R *.fmx}

uses dmOpti, Login_FRM, System.DateUtils, OptiADOHelper; // *** ADDED: The new helper unit ***

procedure TStaffScheduler.FormShow(Sender: TObject);
begin
  ScheduledTasksGrid.OnApplyStyleLookup := GridApplyStyleLookup;
  ScheduledTasksGrid.OnDrawColumnHeader := GridDrawColumnHeader;
  ScheduledTasksGrid.OnDrawColumnCell := GridDrawColumnCell;
  ScheduledTasksGrid.DefaultDrawing := False;
  ScheduledTasksGrid.Options := ScheduledTasksGrid.Options - [TGridOption.Editing];
  ScheduledTasksGrid.OnCellClick := ScheduledTasksGridCellClick;

  StaffAssignGrid.OnApplyStyleLookup := GridApplyStyleLookup;
  StaffAssignGrid.OnDrawColumnHeader := GridDrawColumnHeader;
  StaffAssignGrid.OnDrawColumnCell := GridDrawColumnCell;
  StaffAssignGrid.DefaultDrawing := False;
  StaffAssignGrid.Options := StaffAssignGrid.Options - [TGridOption.Editing];

  cbSelectStaff.OnChange := cbSelectStaffChange;

  FSelectedScheduleID := -1;
  FStaffIDs := TStringList.Create;
  LoadData;
end;

procedure TStaffScheduler.LoadData;
begin
  LoadScheduledTasks;
  LoadStaffAssignments;
  UpdateAvailableStaffForTask;
  rAssignStaff.Enabled := False;
  FSelectedScheduleID := -1;
  ScheduledTasksGrid.Selected := -1;
end;

// *** REFACTORED: Uses the helper to fetch the dataset ***
procedure TStaffScheduler.UpdateAvailableStaffForTask;
var
  qryAvailableStaff: TADOQuery;
  sql: string;
begin
  cbSelectStaff.Items.Clear;
  FStaffIDs.Clear;
  qryAvailableStaff := nil;

  try
    sql := 'SELECT StaffID, Username FROM tblStaff ' +
           'WHERE ManagerID = :pManagerID AND IsActive = False ' +
           'ORDER BY Username';
    qryAvailableStaff := TOptiADOHelper.QueryDataSet(FmOpti.ConOpti, sql, ['pManagerID', CurrentManagerID]);

    while not qryAvailableStaff.Eof do
    begin
      cbSelectStaff.Items.Add(qryAvailableStaff.FieldByName('Username').AsString);
      FStaffIDs.Add(qryAvailableStaff.FieldByName('StaffID').AsString);
      qryAvailableStaff.Next;
    end;
  finally
    qryAvailableStaff.Free;
  end;
end;

// *** REFACTORED: Uses the helper to fetch the dataset ***
procedure TStaffScheduler.LoadScheduledTasks;
var
  qry: TADOQuery;
  i: Integer;
  sql: string;
begin
  ScheduledTasksGrid.RowCount := 0;
  SetLength(FScheduleIDs, 0);
  qry := nil;

  try
    sql := 'SELECT s.ScheduleID, t.TaskName, b.BayName, s.StartTime ' +
           'FROM (tblSchedule s INNER JOIN tblTasks t ON s.TaskID = t.TaskID) ' +
           'INNER JOIN tblBays b ON s.BayID = b.BayID ' +
           'WHERE t.ManagerID = :pManagerID AND s.StaffID IS NULL ' +
           'ORDER BY s.StartTime';
    qry := TOptiADOHelper.QueryDataSet(FmOpti.ConOpti, sql, ['pManagerID', CurrentManagerID]);

    i := 0;
    while not qry.Eof do
    begin
      ScheduledTasksGrid.RowCount := i + 1;
      SetLength(FScheduleIDs, i + 1);
      FScheduleIDs[i] := qry.FieldByName('ScheduleID').AsInteger;
      ScheduledTasksGrid.Cells[colTaskName.Index, i] := qry.FieldByName('TaskName').AsString;
      ScheduledTasksGrid.Cells[colBay.Index, i] := qry.FieldByName('BayName').AsString;
      ScheduledTasksGrid.Cells[colStartTime.Index, i] := FormatDateTime('yyyy-mm-dd hh:nn', qry.FieldByName('StartTime').AsDateTime);
      Inc(i);
      qry.Next;
    end;
  finally
    qry.Free;
  end;
end;

// *** REFACTORED: Uses the helper to fetch the dataset ***
procedure TStaffScheduler.LoadStaffAssignments;
var
  qry: TADOQuery;
  i: Integer;
  sql: string;
begin
  StaffAssignGrid.RowCount := 0;
  qry := nil;

  try
    sql := 'SELECT st.Username, t.TaskName, b.BayName, s.StartTime ' +
           'FROM ((tblSchedule s INNER JOIN tblTasks t ON s.TaskID = t.TaskID) ' +
           'INNER JOIN tblBays b ON s.BayID = b.BayID) ' +
           'INNER JOIN tblStaff st ON s.StaffID = st.StaffID ' +
           'WHERE t.ManagerID = :pManagerID ' +
           'ORDER BY s.StartTime';
    qry := TOptiADOHelper.QueryDataSet(FmOpti.ConOpti, sql, ['pManagerID', CurrentManagerID]);

    i := 0;
    while not qry.Eof do
    begin
      StaffAssignGrid.RowCount := i + 1;
      StaffAssignGrid.Cells[colStaffMember.Index, i] := qry.FieldByName('Username').AsString;
      StaffAssignGrid.Cells[colTaskNameStaff.Index, i] := qry.FieldByName('TaskName').AsString;
      StaffAssignGrid.Cells[colBayStaff.Index, i] := qry.FieldByName('BayName').AsString;
      StaffAssignGrid.Cells[colStartTimeStaff.Index, i] := FormatDateTime('yyyy-mm-dd hh:nn', qry.FieldByName('StartTime').AsDateTime);
      Inc(i);
      qry.Next;
    end;
  finally
    qry.Free;
  end;
end;

procedure TStaffScheduler.ScheduledTasksGridCellClick(const Column: TColumn; const Row: Integer);
begin
  if (Row > -1) and (Row < Length(FScheduleIDs)) then
  begin
    FSelectedScheduleID := FScheduleIDs[Row];
  end
  else
  begin
    FSelectedScheduleID := -1;
  end;
  cbSelectStaff.ItemIndex := -1;
  rAssignStaff.Enabled := False;
end;

procedure TStaffScheduler.cbSelectStaffChange(Sender: TObject);
begin
  rAssignStaff.Enabled := (FSelectedScheduleID > 0) and (cbSelectStaff.ItemIndex > -1);
end;

// *** REFACTORED: Uses the helper for clean, single-line database updates ***
procedure TStaffScheduler.rAssignStaffClick(Sender: TObject);
var
  lStaffID: Integer;
  sql: string;
begin
  if FSelectedScheduleID < 1 then
  begin
    ShowMessage('Please select a task from the "Scheduled Tasks" grid.');
    Exit;
  end;

  if cbSelectStaff.ItemIndex < 0 then
  begin
    ShowMessage('Please select a staff member from the dropdown list.');
    Exit;
  end;

  lStaffID := StrToInt(FStaffIDs[cbSelectStaff.ItemIndex]);

  FmOpti.ConOpti.BeginTrans;
  try
    // Step 1: Update the schedule table with the StaffID
    sql := 'UPDATE tblSchedule SET StaffID = :pStaffID WHERE ScheduleID = :pScheduleID';
    TOptiADOHelper.ExecSQL(FmOpti.ConOpti, sql, ['pStaffID', lStaffID, 'pScheduleID', FSelectedScheduleID]);

    // Step 2: Update the staff table to set IsActive to True
    sql := 'UPDATE tblStaff SET IsActive = True WHERE StaffID = :pStaffID';
    TOptiADOHelper.ExecSQL(FmOpti.ConOpti, sql, ['pStaffID', lStaffID]);

    FmOpti.ConOpti.CommitTrans;
    ShowMessage('Staff member assigned successfully.');
  except
    on E: Exception do
    begin
      FmOpti.ConOpti.RollbackTrans;
      ShowMessage('An error occurred while assigning the staff member: ' + E.Message);
    end;
  end;

  LoadData;
end;

// --- Grid Styling Procedures ---

procedure TStaffScheduler.GridApplyStyleLookup(Sender: TObject);
var
  LBackground: TFmxObject;
  Grid: TStringGrid;
begin
  Grid := Sender as TStringGrid;
  LBackground := Grid.FindStyleResource('background');
  if (LBackground is TControl) then
    (LBackground as TControl).Visible := False;
end;

procedure TStaffScheduler.GridDrawColumnHeader(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF);
begin
  Canvas.Fill.Color := TAlphaColor($FF1F272C);
  Canvas.FillRect(Bounds, 0, 0, [], 1.0);
  Canvas.Fill.Color := TAlphaColor($FFF8F9FA);
  Canvas.FillText(Bounds, Column.Header, False, 1.0, [], TTextAlign.Center, TTextAlign.Center);
end;

procedure TStaffScheduler.GridDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
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

end.
