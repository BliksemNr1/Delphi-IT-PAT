program OptiflowPAT;

uses
  System.StartUpCopy,
  FMX.Forms,
  Login_Frm in 'Login_Frm.pas' {FLogin},
  SignUp_FRM in 'SignUp_FRM.pas' {FSignUp},
  LoginQR_FRM in 'LoginQR_FRM.pas' {Form1},
  Menu_FRM in 'Menu_FRM.pas' {fmenu},
  Dashboard_FRM in 'Dashboard_FRM.pas' {fDashboard},
  BayManager_FRM in 'BayManager_FRM.pas' {Baymanager},
  TaskManagement_FRM in 'TaskManagement_FRM.pas' {FTaskManagement},
  StaffScheduler_FRM in 'StaffScheduler_FRM.pas' {StaffScheduler},
  StaffManager_FRM in 'StaffManager_FRM.pas' {Staffmanager},
  Reports_FRM in 'Reports_FRM.pas' {Reports},
  dmOpti in 'dmOpti.pas' {FmOpti: TDataModule},
  Scheduler_FRM in 'Scheduler_FRM.pas' {Scheduler},
  OptiADOhelper in 'OptiADOhelper.pas',
  NotificationBell_Frame in 'NotificationBell_Frame.pas' {rNotiBell: TFrame},
  Notifications_FRM in 'Notifications_FRM.pas' {FNotifications};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFLogin, FLogin);
  Application.CreateForm(TFSignUp, FSignUp);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(Tfmenu, fmenu);
  Application.CreateForm(TfDashboard, fDashboard);
  Application.CreateForm(TBaymanager, Baymanager);
  Application.CreateForm(TFTaskManagement, FTaskManagement);
  Application.CreateForm(TStaffScheduler, StaffScheduler);
  Application.CreateForm(TStaffmanager, Staffmanager);
  Application.CreateForm(TReports, Reports);
  Application.CreateForm(TFmOpti, FmOpti);
  Application.CreateForm(TScheduler, Scheduler);
  Application.CreateForm(TFNotifications, FNotifications);
  Application.Run;
end.
