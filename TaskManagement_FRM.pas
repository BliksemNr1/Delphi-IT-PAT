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
    labels, Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    lbltname: TEdit;
    lblDuration: TEdit;
    cbPriority: TComboBox;
    StyleBook1: TStyleBook;
    colTaskName: TStringColumn;
    colDuration: TStringColumn;
    colPriority: TStringColumn;
    colBayType: TStringColumn;
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
end;

procedure TFTaskManagement.PopulateBayTypeCombo;
begin
  cbbayType.Items.Clear;
  cbbayType.Items.Add('Crane');
  cbbayType.Items.Add('Fast Load');
  cbbayType.Items.Add('Standard');
  cbbayType.ItemIndex := -1;
end;

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

procedure TFTaskManagement.LoadTasksToGrid;
var
  i: Integer;
begin
  StringGrid1.RowCount := 0;
  SetLength(FTaskIDs, 0);
  FmOpti.optiTasks.First;

  i := 0;
  while not FmOpti.optiTasks.Eof do
  begin
    if FmOpti.optiTasks.FieldByName('ManagerID').AsInteger = CurrentManagerID then
    begin
      StringGrid1.RowCount := i + 1;
      SetLength(FTaskIDs, i + 1);
      FTaskIDs[i] := FmOpti.optiTasks.FieldByName('TaskID').AsInteger;
      StringGrid1.Cells[0, i] := FmOpti.optiTasks.FieldByName('TaskName').AsString;
      StringGrid1.Cells[1, i] := FmOpti.optiTasks.FieldByName('Duration').AsString;
      StringGrid1.Cells[2, i] := FmOpti.optiTasks.FieldByName('Priority').AsString;
      StringGrid1.Cells[3, i] := FmOpti.optiTasks.FieldByName('BayType').AsString;
      Inc(i);
    end;
    FmOpti.optiTasks.Next;
  end;
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
  if (lbltname.Text = '') or (cbPriority.ItemIndex = -1) or (cbbayType.ItemIndex = -1) then
  begin
    ShowMessage('Please fill in all fields.');
    Exit;
  end;

  FmOpti.optiTasks.Append;
  FmOpti.optiTasks.FieldByName('TaskName').AsString := lbltname.Text;
  FmOpti.optiTasks.FieldByName('Duration').AsInteger := StrToIntDef(lblDuration.Text, 0);
  FmOpti.optiTasks.FieldByName('Priority').AsString := cbPriority.Text;
  FmOpti.optiTasks.FieldByName('BayType').AsString := cbbayType.Text;
  // *** FIX 1: New tasks should be 'Pending', not 'Scheduled'. ***
  FmOpti.optiTasks.FieldByName('Status').AsString := 'Pending';
  // *** FIX 2: Do NOT set a StartTime. The scheduler does this. ***
  // FmOpti.optiTasks.FieldByName('StartTime').AsDateTime := Now; // This line was incorrect.
  FmOpti.optiTasks.FieldByName('ManagerID').AsInteger := CurrentManagerID;
  FmOpti.optiTasks.Post;

  LoadTasksToGrid;
  ClearFields;
  ShowMessage('Task added successfully.');
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
    FmOpti.optiTasks.Edit;
    FmOpti.optiTasks.FieldByName('TaskName').AsString := lbltname.Text;
    FmOpti.optiTasks.FieldByName('Duration').AsInteger := StrToIntDef(lblDuration.Text, 0);
    FmOpti.optiTasks.FieldByName('Priority').AsString := cbPriority.Text;
    FmOpti.optiTasks.FieldByName('BayType').AsString := cbbayType.Text;
    FmOpti.optiTasks.Post;

    LoadTasksToGrid;
    ClearFields;
    ShowMessage('Task updated successfully.');
  end
  else
  begin
    ShowMessage('Error: Could not find the selected task in the database.');
  end;
end;

procedure TFTaskManagement.StringGrid1CellClick(const Column: TColumn; const Row: Integer);
begin
  if Row < 0 then Exit;

  SelectedTaskRow := Row;

  lbltname.Text := StringGrid1.Cells[0, Row];
  lblDuration.Text := StringGrid1.Cells[1, Row];
  cbPriority.ItemIndex := cbPriority.Items.IndexOf(StringGrid1.Cells[2, Row]);
  cbbayType.ItemIndex := cbbayType.Items.IndexOf(StringGrid1.Cells[3, Row]);
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


