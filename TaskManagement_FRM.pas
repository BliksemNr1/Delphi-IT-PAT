unit TaskManagement_FRM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects, System.Rtti, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid, FMX.StdCtrls, FMX.ListBox, FMX.DateTimeCtrls;

type
  TFTaskManagement = class(TForm)
    rEdit: TRectangle;
    radd: TRectangle;
    StringGrid1: TStringGrid;
    labels, Label1, Label2, Label3, Label4, Label5, Label6, Label7: TLabel; // Added Label7 for Deadline
    lbltname: TEdit;
    lblDuration: TEdit;
    cbPriority: TComboBox;
    StyleBook1: TStyleBook;
    colTaskName: TStringColumn;
    colDuration: TStringColumn;
    colPriority: TStringColumn;
    colBayType: TStringColumn;
    colDeadline: TStringColumn; // Added column for Deadline
    cbbayType: TComboBox;
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
    dtpDeadline: TDateEdit; // Assumed TDateEdit component for the deadline
    procedure FormShow(Sender: TObject);
    procedure raddClick(Sender: TObject);
    procedure rEditClick(Sender: TObject);
    procedure rDeleteClick(Sender: TObject);
    procedure StringGrid1CellClick(const Column: TColumn; const Row: Integer);
    procedure StringGrid1DrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure StringGrid1DrawColumnHeader(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF);
    procedure StringGrid1ApplyStyleLookup(Sender: TObject);
  private
    { Private declarations }
    SelectedTaskRow: Integer;
    FTaskIDs: TArray<Integer>;
    procedure LoadTasksToGrid;
    procedure ClearFields;
    procedure PopulatePriorityCombo;
    procedure PopulateBayTypeCombo;
  public
    { Public declarations }
  end;

var
  FTaskManagement: TFTaskManagement;

implementation

{$R *.fmx}

uses
  dmOpti, Login_FRM;

procedure TFTaskManagement.ClearFields;
begin
  lbltname.Text := '';
  lblDuration.Text := '';
  cbPriority.ItemIndex := -1;
  cbbayType.ItemIndex := -1;
  dtpDeadline.Date := Now; // Reset deadline to current date
  SelectedTaskRow := -1;
  StringGrid1.Selected := -1;
end;

procedure TFTaskManagement.FormShow(Sender: TObject);
begin
  StringGrid1.OnCellClick := StringGrid1CellClick;
  StringGrid1.OnApplyStyleLookup := StringGrid1ApplyStyleLookup;
  StringGrid1.OnDrawColumnCell := StringGrid1DrawColumnCell;
  StringGrid1.OnDrawColumnHeader := StringGrid1DrawColumnHeader;
  StringGrid1.DefaultDrawing := False;
  StringGrid1.Options := StringGrid1.Options - [TGridOption.Editing];
  SelectedTaskRow := -1;
  PopulatePriorityCombo;
  PopulateBayTypeCombo;
  if not FmOpti.optiTasks.Active then
    FmOpti.optiTasks.Open;
  LoadTasksToGrid;
  ClearFields; // Clear fields on form show
end;

procedure TFTaskManagement.LoadTasksToGrid;
var
  i: Integer;
  DeadlineValue: TDateTime;
begin
  StringGrid1.RowCount := 0;
  SetLength(FTaskIDs, 0);

  // Ensure the dataset is open and filtered
  FmOpti.optiTasks.Close;
  FmOpti.optiTasks.Filter := 'ManagerID = ' + IntToStr(CurrentManagerID);
  FmOpti.optiTasks.Filtered := True;
  FmOpti.optiTasks.Open;

  FmOpti.optiTasks.First;
  i := 0;
  while not FmOpti.optiTasks.Eof do
  begin
    StringGrid1.RowCount := i + 1;
    SetLength(FTaskIDs, i + 1);
    FTaskIDs[i] := FmOpti.optiTasks.FieldByName('TaskID').AsInteger;

    StringGrid1.Cells[colTaskName.Index, i] := FmOpti.optiTasks.FieldByName('TaskName').AsString;
    StringGrid1.Cells[colDuration.Index, i] := FmOpti.optiTasks.FieldByName('Duration').AsString;
    StringGrid1.Cells[colPriority.Index, i] := FmOpti.optiTasks.FieldByName('Priority').AsString;
    StringGrid1.Cells[colBayType.Index, i] := FmOpti.optiTasks.FieldByName('BayType').AsString;

    // Load and format the deadline
    if not FmOpti.optiTasks.FieldByName('Deadline').IsNull then
    begin
      DeadlineValue := FmOpti.optiTasks.FieldByName('Deadline').AsDateTime;
      StringGrid1.Cells[colDeadline.Index, i] := FormatDateTime('yyyy-mm-dd', DeadlineValue);
    end
    else
    begin
      StringGrid1.Cells[colDeadline.Index, i] := 'Not Set';
    end;

    Inc(i);
    FmOpti.optiTasks.Next;
  end;
end;

procedure TFTaskManagement.PopulateBayTypeCombo;
begin
  cbbayType.Items.Clear;
  cbbayType.Items.Add('Crane');
  cbbayType.Items.Add('Fast Load');
  cbbayType.Items.Add('Standard');
  cbbayType.ItemIndex := -1;
end;

procedure TFTaskManagement.PopulatePriorityCombo;
begin
  cbPriority.Items.Clear;
  cbPriority.Items.Add('Low');
  cbPriority.Items.Add('Medium');
  cbPriority.Items.Add('High');
  cbPriority.ItemIndex := -1;
end;

procedure TFTaskManagement.raddClick(Sender: TObject);
begin
  // --- Input Validation ---
  if (lbltname.Text = '') or (cbPriority.ItemIndex = -1) or
     (cbbayType.ItemIndex = -1) or (lblDuration.Text = '') then
  begin
    ShowMessage('Please fill in all fields, including Task Name, Duration, Priority, and Bay Type.');
    Exit;
  end;

  try
    FmOpti.optiTasks.Append;
    FmOpti.optiTasks.FieldByName('TaskName').AsString := lbltname.Text;
    FmOpti.optiTasks.FieldByName('Duration').AsInteger := StrToIntDef(lblDuration.Text, 0);
    FmOpti.optiTasks.FieldByName('Priority').AsString := cbPriority.Text;
    FmOpti.optiTasks.FieldByName('BayType').AsString := cbbayType.Text;
    FmOpti.optiTasks.FieldByName('Status').AsString := 'Pending';
    FmOpti.optiTasks.FieldByName('ManagerID').AsInteger := CurrentManagerID;
    // --- Save the Deadline ---
    FmOpti.optiTasks.FieldByName('Deadline').AsDateTime := dtpDeadline.Date;
    FmOpti.optiTasks.Post;

    LoadTasksToGrid;
    ClearFields;
    ShowMessage('Task added successfully.');
  except
    on E: Exception do
      ShowMessage('An error occurred while adding the task: ' + E.Message);
  end;
end;

procedure TFTaskManagement.rDeleteClick(Sender: TObject);
var
  LTaskID: Integer;
begin
  if SelectedTaskRow < 0 then
  begin
    ShowMessage('Please select a task to delete.');
    Exit;
  end;

  LTaskID := FTaskIDs[SelectedTaskRow];

  if FmOpti.optiTasks.Locate('TaskID', LTaskID, []) then
  begin
    if MessageDlg('Are you sure you want to delete this task?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      FmOpti.optiTasks.Delete;
      LoadTasksToGrid;
      ClearFields;
      ShowMessage('Task deleted successfully.');
    end;
  end
  else
  begin
    ShowMessage('Error: Could not find the selected task to delete.');
  end;
end;

procedure TFTaskManagement.rEditClick(Sender: TObject);
var
  LTaskID: Integer;
begin
  if SelectedTaskRow < 0 then
  begin
    ShowMessage('Please select a task to edit.');
    Exit;
  end;

  LTaskID := FTaskIDs[SelectedTaskRow];

  if FmOpti.optiTasks.Locate('TaskID', LTaskID, []) then
  begin
    try
      FmOpti.optiTasks.Edit;
      FmOpti.optiTasks.FieldByName('TaskName').AsString := lbltname.Text;
      FmOpti.optiTasks.FieldByName('Duration').AsInteger := StrToIntDef(lblDuration.Text, 0);
      FmOpti.optiTasks.FieldByName('Priority').AsString := cbPriority.Text;
      FmOpti.optiTasks.FieldByName('BayType').AsString := cbbayType.Text;
      // --- Update the Deadline ---
      FmOpti.optiTasks.FieldByName('Deadline').AsDateTime := dtpDeadline.Date;
      FmOpti.optiTasks.Post;

      LoadTasksToGrid;
      ClearFields;
      ShowMessage('Task updated successfully.');
    except
      on E: Exception do
        ShowMessage('An error occurred while updating the task: ' + E.Message);
    end;
  end
  else
  begin
    ShowMessage('Error: Could not find the selected task in the database.');
  end;
end;

procedure TFTaskManagement.StringGrid1CellClick(const Column: TColumn; const Row: Integer);
var
  DeadlineStr: string;
  DeadlineValue: TDateTime;
begin
  if Row < 0 then Exit;

  SelectedTaskRow := Row;

  // Populate the text fields from the grid
  lbltname.Text := StringGrid1.Cells[colTaskName.Index, Row];
  lblDuration.Text := StringGrid1.Cells[colDuration.Index, Row];
  cbPriority.ItemIndex := cbPriority.Items.IndexOf(StringGrid1.Cells[colPriority.Index, Row]);
  cbbayType.ItemIndex := cbbayType.Items.IndexOf(StringGrid1.Cells[colBayType.Index, Row]);

  // --- Load the Deadline into the TDateEdit ---
  DeadlineStr := StringGrid1.Cells[colDeadline.Index, Row];
  if TryStrToDate(DeadlineStr, DeadlineValue) then
  begin
    dtpDeadline.Date := DeadlineValue;
  end
  else
  begin
    // If the deadline is 'Not Set' or invalid, default to today
    dtpDeadline.Date := Now;
  end;
end;

// --- No changes needed for the drawing procedures ---

procedure TFTaskManagement.StringGrid1ApplyStyleLookup(Sender: TObject);
var
  LBackground: TFmxObject;
begin
  LBackground := StringGrid1.FindStyleResource('background');
  if (LBackground is TControl) then
  begin
    (LBackground as TControl).Visible := False;
  end;
end;

procedure TFTaskManagement.StringGrid1DrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
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

procedure TFTaskManagement.StringGrid1DrawColumnHeader(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF);
begin
  Canvas.Fill.Color := TAlphaColor($FF1F272C);
  Canvas.FillRect(Bounds, 0, 0, [], 1.0);

  Canvas.Fill.Color := TAlphaColor($FFF8F9FA);
  Canvas.FillText(Bounds, Column.Header, False, 1.0, [], TTextAlign.Center, TTextAlign.Center);
end;

end.

