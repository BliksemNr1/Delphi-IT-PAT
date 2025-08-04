unit BayManager_FRM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects, System.Rtti, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid, FMX.StdCtrls, FMX.ListBox, Data.DB, Data.Win.ADODB;

type
  TBayManager = class(TForm)
    Rectangle17: TRectangle;
    labels: TLabel;
    lbledtBayName: TEdit;
    Rectangle19: TRectangle;
    Label2: TLabel;
    cmbBayType: TComboBox;
    StyleBook1: TStyleBook;
    Status: TLabel;
    StringGrid1: TStringGrid;
    colBayName: TStringColumn;
    colBayType: TStringColumn;
    colStatus: TStringColumn;
    Rectangle18: TRectangle;
    cmbStatus: TComboBox;
    rDeleteBay: TRectangle;
    rEditBay: TRectangle;
    raddbay: TRectangle;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
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
    procedure raddbayClick(Sender: TObject);
    procedure rEditBayClick(Sender: TObject);
    procedure rDeleteBayClick(Sender: TObject);
    procedure StringGrid1CellClick(const Column: TColumn; const Row: Integer);
    procedure StringGrid1DrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure StringGrid1DrawColumnHeader(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF);
    procedure StringGrid1ApplyStyleLookup(Sender: TObject);
  private
    { Private declarations }
    SelectedBayRow: Integer;
    FBayIDs: TArray<Integer>;
    procedure LoadBaysToGrid;
    procedure ClearFields;
    procedure PopulateComboBoxes;
    function IsBayNameUnique(const ABayName: string; AExcludingBayID: Integer = -1): Boolean;
  public
    { Public declarations }
  end;

var
  BayManager: TBayManager;

implementation

{$R *.fmx}

uses dmOpti, Login_FRM;

procedure TBayManager.ClearFields;
begin
  lbledtBayName.Text := '';
  cmbBayType.ItemIndex := -1;
  cmbStatus.ItemIndex := -1;
  SelectedBayRow := -1;
  StringGrid1.Selected := -1;
end;

procedure TBayManager.FormCreate(Sender: TObject);
begin
  StringGrid1.OnCellClick := StringGrid1CellClick;
  StringGrid1.OnApplyStyleLookup := StringGrid1ApplyStyleLookup;
  StringGrid1.OnDrawColumnCell := StringGrid1DrawColumnCell;
  StringGrid1.OnDrawColumnHeader := StringGrid1DrawColumnHeader;
  StringGrid1.DefaultDrawing := False;
  StringGrid1.Options := StringGrid1.Options - [TGridOption.Editing];

  SelectedBayRow := -1;
  PopulateComboBoxes;

  if not FmOpti.optiBays.Active then
    FmOpti.optiBays.Open;

  LoadBaysToGrid;
end;

function TBayManager.IsBayNameUnique(const ABayName: string; AExcludingBayID: Integer = -1): Boolean;
var
  qryCheck: TADOQuery;
begin
  Result := False;
  qryCheck := TADOQuery.Create(nil);
  try
    qryCheck.Connection := FmOpti.ConOpti;
    qryCheck.SQL.Text := 'SELECT COUNT(*) FROM tblBays ' +
                       'WHERE BayName = :pBayName AND BayID <> :pExcludingBayID AND ManagerID = :pManagerID';
    qryCheck.Parameters.ParamByName('pBayName').Value := ABayName;
    qryCheck.Parameters.ParamByName('pExcludingBayID').Value := AExcludingBayID;
    qryCheck.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    qryCheck.Open;
    Result := qryCheck.Fields[0].AsInteger = 0;
  finally
    qryCheck.Free;
  end;
end;

procedure TBayManager.LoadBaysToGrid;
var
  i: Integer;
begin
  StringGrid1.RowCount := 0;
  SetLength(FBayIDs, 0);
  FmOpti.optiBays.First;

  i := 0;
  while not FmOpti.optiBays.Eof do
  begin
    if FmOpti.optiBays.FieldByName('ManagerID').AsInteger = CurrentManagerID then
    begin
      StringGrid1.RowCount := i + 1;
      SetLength(FBayIDs, i + 1);
      FBayIDs[i] := FmOpti.optiBays.FieldByName('BayID').AsInteger;
      StringGrid1.Cells[0, i] := FmOpti.optiBays.FieldByName('BayName').AsString;
      StringGrid1.Cells[1, i] := FmOpti.optiBays.FieldByName('BayType').AsString;
      StringGrid1.Cells[2, i] := FmOpti.optiBays.FieldByName('Status').AsString;
      Inc(i);
    end;
    FmOpti.optiBays.Next;
  end;
end;

procedure TBayManager.PopulateComboBoxes;
begin
  cmbBayType.Items.Clear;
  cmbBayType.Items.Add('Crane');
  cmbBayType.Items.Add('Fast Load');
  cmbBayType.Items.Add('Standard');
  cmbBayType.ItemIndex := -1;

  cmbStatus.Items.Clear;
  cmbStatus.Items.Add('Available');
  cmbStatus.Items.Add('In Use');
  cmbStatus.Items.Add('Maintenance');
  cmbStatus.ItemIndex := -1;
end;

procedure TBayManager.raddbayClick(Sender: TObject);
begin
  if (lbledtBayName.Text = '') or (cmbBayType.ItemIndex = -1) or (cmbStatus.ItemIndex = -1) then
  begin
    ShowMessage('Please fill in all fields to add a new bay.');
    Exit;
  end;

  if not IsBayNameUnique(lbledtBayName.Text) then
  begin
    ShowMessage('Error: A bay with the name "' + lbledtBayName.Text + '" already exists. Please use a unique name.');
    Exit;
  end;

  try
    FmOpti.optiBays.Append;
    FmOpti.optiBays.FieldByName('BayName').AsString := lbledtBayName.Text;
    FmOpti.optiBays.FieldByName('BayType').AsString := cmbBayType.Text;
    FmOpti.optiBays.FieldByName('Status').AsString := cmbStatus.Text;
    FmOpti.optiBays.FieldByName('ManagerID').AsInteger := CurrentManagerID;
    FmOpti.optiBays.Post;

    LoadBaysToGrid;
    ClearFields;
    ShowMessage('Bay added successfully.');
  except
    on E: Exception do
    begin
      ShowMessage('A database error occurred while adding the bay: ' + E.Message);
    end;
  end;
end;

procedure TBayManager.rDeleteBayClick(Sender: TObject);
var
  LBayID: Integer;
begin
  if SelectedBayRow < 0 then
  begin
    ShowMessage('Please select a bay from the grid to delete.');
    Exit;
  end;

  if MessageDlg('Are you sure you want to delete this bay?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrNo then
    Exit;

  LBayID := FBayIDs[SelectedBayRow];

  try
    if FmOpti.optiBays.Locate('BayID', LBayID, []) then
    begin
      FmOpti.optiBays.Delete;
      LoadBaysToGrid;
      ClearFields;
      ShowMessage('Bay deleted successfully.');
    end
    else
    begin
      ShowMessage('Error: Could not find the selected bay to delete.');
    end;
  except
    on E: Exception do
    begin
      ShowMessage('A database error occurred while deleting the bay: ' + E.Message);
    end;
  end;
end;

procedure TBayManager.rEditBayClick(Sender: TObject);
var
  LBayID: Integer;
begin
  if SelectedBayRow < 0 then
  begin
    ShowMessage('Please select a bay from the grid to edit.');
    Exit;
  end;

  if (lbledtBayName.Text = '') or (cmbBayType.ItemIndex = -1) or (cmbStatus.ItemIndex = -1) then
  begin
    ShowMessage('Please ensure all fields are filled correctly.');
    Exit;
  end;

  LBayID := FBayIDs[SelectedBayRow];

  if not IsBayNameUnique(lbledtBayName.Text, LBayID) then
  begin
    ShowMessage('Error: Another bay with the name "' + lbledtBayName.Text + '" already exists. Please use a unique name.');
    Exit;
  end;

  try
    if FmOpti.optiBays.Locate('BayID', LBayID, []) then
    begin
      FmOpti.optiBays.Edit;
      FmOpti.optiBays.FieldByName('BayName').AsString := lbledtBayName.Text;
      FmOpti.optiBays.FieldByName('BayType').AsString := cmbBayType.Text;
      FmOpti.optiBays.FieldByName('Status').AsString := cmbStatus.Text;
      FmOpti.optiBays.Post;

      LoadBaysToGrid;
      ClearFields;
      ShowMessage('Bay updated successfully.');
    end
    else
    begin
      ShowMessage('Error: Could not find the selected bay in the database.');
    end;
  except
    on E: Exception do
    begin
       ShowMessage('A database error occurred while updating the bay: ' + E.Message);
    end;
  end;
end;

procedure TBayManager.StringGrid1CellClick(const Column: TColumn; const Row: Integer);
begin
  if Row < 0 then Exit;
  SelectedBayRow := Row;
  lbledtBayName.Text := StringGrid1.Cells[0, Row];
  cmbBayType.ItemIndex := cmbBayType.Items.IndexOf(StringGrid1.Cells[1, Row]);
  cmbStatus.ItemIndex := cmbStatus.Items.IndexOf(StringGrid1.Cells[2, Row]);
end;

procedure TBayManager.StringGrid1ApplyStyleLookup(Sender: TObject);
var
  LBackground: TFmxObject;
begin
  LBackground := StringGrid1.FindStyleResource('background');
  if (LBackground is TControl) then
  begin
    (LBackground as TControl).Visible := False;
  end;
end;

procedure TBayManager.StringGrid1DrawColumnCell(Sender: TObject;
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

procedure TBayManager.StringGrid1DrawColumnHeader(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF);
begin
  Canvas.Fill.Color := TAlphaColor($FF1F272C);
  Canvas.FillRect(Bounds, 0, 0, [], 1.0);

  Canvas.Fill.Color := TAlphaColor($FFF8F9FA);
  Canvas.FillText(Bounds, Column.Header, False, 1.0, [], TTextAlign.Center, TTextAlign.Center);
end;

end.
