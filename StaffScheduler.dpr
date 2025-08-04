program StaffScheduler;

uses
  System.StartUpCopy,
  FMX.Forms,
  Login_Frm in 'Login_Frm.pas' {FLogin},
  SignUp_FRM in 'SignUp_FRM.pas' {FSignUp},
  LoginQR_FRM in 'LoginQR_FRM.pas' {Form1},
  Menu_FRM in 'Menu_FRM.pas' {Form2},
  Dashboard_FRM in 'Dashboard_FRM.pas' {Form3},
  BayManager_FRM in 'BayManager_FRM.pas' {Form4},
  TaskManagement_FRM in 'TaskManagement_FRM.pas' {Form5},
  StaffScheduler_FRM in 'StaffScheduler_FRM.pas' {Form6},
  StaffManager_FRM in 'StaffManager_FRM.pas' {Form7},
  ExportData_FRM in 'ExportData_FRM.pas' {Form8},
  dmOpti in 'dmOpti.pas' {FmOpti: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFLogin, FLogin);
  Application.CreateForm(TFSignUp, FSignUp);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TForm6, Form6);
  Application.CreateForm(TForm7, Form7);
  Application.CreateForm(TForm8, Form8);
  Application.CreateForm(TFmOpti, FmOpti);
  Application.Run;
end.
