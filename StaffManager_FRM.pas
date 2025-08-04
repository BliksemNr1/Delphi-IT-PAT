unit StaffManager_FRM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects, FMX.ListBox, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox;

type
  TStaffManager = class(TForm)
    Rectangle17: TRectangle;
    Rectangle18: TRectangle;
    Rectangle19: TRectangle;
    labels: TLabel;
    Label1: TLabel;
    Role: TLabel;
    lblFullName: TEdit;
    lblPassword: TEdit;
    cbRole: TComboBox;
    StyleBook1: TStyleBook;
    Rectangle21: TRectangle;
    StringGrid1: TStringGrid;
    colName: TStringColumn;
    colRole: TStringColumn;
    rDeletestaff: TRectangle;
    Label6: TLabel;
    rEditStaff: TRectangle;
    Label5: TLabel;
    raddstaff: TRectangle;
    Label4: TLabel;
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
    procedure rAddStaffClick(Sender: TObject);
    procedure rEditStaffClick(Sender: TObject);
    procedure rDeleteStaffClick(Sender: TObject);
    procedure StringGrid1CellClick(const Column: TColumn; const Row: Integer);
    procedure StringGrid1DrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure StringGrid1DrawColumnHeader(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF);
    procedure StringGrid1ApplyStyleLookup(Sender: TObject);
  private
    { Private declarations }
    SelectedStaffRow: Integer;
    FStaffIDs: TArray<Integer>;
    procedure LoadStaffToGrid;
    procedure ClearFields;
    procedure PopulateRoleCombo;
  public
    { Public declarations }
  end;

var
  StaffManager: TStaffManager;

implementation

{$R *.fmx}

uses dmOpti, Login_FRM;

procedure TStaffManager.FormShow(Sender: TObject);
begin
  StringGrid1.OnCellClick := StringGrid1CellClick;
  StringGrid1.OnApplyStyleLookup := StringGrid1ApplyStyleLookup;
  StringGrid1.OnDrawColumnCell := StringGrid1DrawColumnCell;
  StringGrid1.OnDrawColumnHeader := StringGrid1DrawColumnHeader;
  StringGrid1.DefaultDrawing := False;
  StringGrid1.Options := StringGrid1.Options - [TGridOption.Editing];

  SelectedStaffRow := -1;
  PopulateRoleCombo;

  if not FmOpti.optiStaff.Active then
    FmOpti.optiStaff.Open;
  LoadStaffToGrid;
end;

procedure TStaffManager.ClearFields;
begin
  lblFullName.Text := '';
  lblPassword.Text := '';
  cbRole.ItemIndex := -1;
  SelectedStaffRow := -1;
  StringGrid1.Selected := -1;
end;

procedure TStaffManager.PopulateRoleCombo;
begin
  // *** UPDATED: Roles are now more specific to the application's function ***
  cbRole.Items.Clear;
  cbRole.Items.Add('Dispatcher');
  cbRole.Items.Add('Supervisor');
  cbRole.Items.Add('Ground Crew / Operator');
  cbRole.ItemIndex := -1;
end;

procedure TStaffManager.LoadStaffToGrid;
var
  i: Integer;
begin
  StringGrid1.RowCount := 0;
  SetLength(FStaffIDs, 0);
  FmOpti.optiStaff.First;

  i := 0;
  while not FmOpti.optiStaff.Eof do
  begin
    if FmOpti.optiStaff.FieldByName('ManagerID').AsInteger = CurrentManagerID then
    begin
      StringGrid1.RowCount := i + 1;
      SetLength(FStaffIDs, i + 1);
      FStaffIDs[i] := FmOpti.optiStaff.FieldByName('StaffID').AsInteger;
      StringGrid1.Cells[0, i] := FmOpti.optiStaff.FieldByName('Username').AsString;
      StringGrid1.Cells[1, i] := FmOpti.optiStaff.FieldByName('Role').AsString;
      Inc(i);
    end;
    FmOpti.optiStaff.Next;
  end;
end;

procedure TStaffManager.StringGrid1CellClick(const Column: TColumn; const Row: Integer);
begin
  if Row < 0 then Exit;

  SelectedStaffRow := Row;

  lblFullName.Text := StringGrid1.Cells[0, Row];
  cbRole.ItemIndex := cbRole.Items.IndexOf(StringGrid1.Cells[1, Row]);
  lblPassword.Text := '';
end;

procedure TStaffManager.rAddStaffClick(Sender: TObject);
begin
  if (lblFullName.Text = '') or (lblPassword.Text = '') or (cbRole.ItemIndex = -1) then
  begin
    ShowMessage('Please fill in all fields to add a new staff member.');
    Exit;
  end;

  FmOpti.optiStaff.Append;
  FmOpti.optiStaff.FieldByName('Username').AsString := lblFullName.Text;
  FmOpti.optiStaff.FieldByName('Password').AsString := lblPassword.Text;
  FmOpti.optiStaff.FieldByName('Role').AsString := cbRole.Text;
  FmOpti.optiStaff.FieldByName('IsActive').AsBoolean := True;
  FmOpti.optiStaff.FieldByName('ManagerID').AsInteger := CurrentManagerID;
  FmOpti.optiStaff.Post;

  LoadStaffToGrid;
  ClearFields;
  ShowMessage('Staff member added successfully.');
end;

procedure TStaffManager.rEditStaffClick(Sender: TObject);
var
  LStaffID: Integer;
begin
  if SelectedStaffRow < 0 then
  begin
    ShowMessage('Please select a staff member from the grid to edit.');
    Exit;
  end;

  LStaffID := FStaffIDs[SelectedStaffRow];

  if FmOpti.optiStaff.Locate('StaffID', LStaffID, []) then
  begin
    FmOpti.optiStaff.Edit;
    FmOpti.optiStaff.FieldByName('Username').AsString := lblFullName.Text;
    FmOpti.optiStaff.FieldByName('Role').AsString := cbRole.Text;
    if lblPassword.Text <> '' then
      FmOpti.optiStaff.FieldByName('Password').AsString := lblPassword.Text;
    FmOpti.optiStaff.Post;

    LoadStaffToGrid;
    ClearFields;
    ShowMessage('Staff member updated successfully.');
  end
  else
  begin
    ShowMessage('Error: Could not find the selected staff member.');
  end;
end;

procedure TStaffManager.rDeleteStaffClick(Sender: TObject);
var
  LStaffID: Integer;
begin
  if SelectedStaffRow < 0 then
  begin
    ShowMessage('Please select a staff member to remove.');
    Exit;
  end;

  LStaffID := FStaffIDs[SelectedStaffRow];

  if FmOpti.optiStaff.Locate('StaffID', LStaffID, []) then
  begin
    if MessageDlg('Are you sure you want to remove this staff member?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
        FmOpti.optiStaff.Delete;
        LoadStaffToGrid;
        ClearFields;
        ShowMessage('Staff member removed successfully.');
    end;
  end
  else
  begin
    ShowMessage('Error: Could not find the selected staff member to remove.');
  end;
end;

procedure TStaffManager.StringGrid1ApplyStyleLookup(Sender: TObject);
var
  LBackground: TFmxObject;
begin
  LBackground := StringGrid1.FindStyleResource('background');
  if (LBackground is TControl) then
  begin
    (LBackground as TControl).Visible := False;
  end;
end;

procedure TStaffManager.StringGrid1DrawColumnHeader(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF);
begin
  Canvas.Fill.Color := TAlphaColor($FF1F272C);
  Canvas.FillRect(Bounds, 0, 0, [], 1.0);
  Canvas.Fill.Color := TAlphaColor($FFF8F9FA);
  Canvas.FillText(Bounds, Column.Header, False, 1.0, [], TTextAlign.Center, TTextAlign.Center);
end;

procedure TStaffManager.StringGrid1DrawColumnCell(Sender: TObject;
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

end.


