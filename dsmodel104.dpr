library dsmodel104;
  {-De totale hardheid van het infiltrerende water (mmol/l) vlgs.
    'Beschrijving SPREAD'. Ir. W. Beekman, KIWA 1998. Opdrachtnr. 11.1154.010.
    ECHTER: de antropogene invloed op de totale hardheid is beschreven met een
    lineaire relatie waarin de SO4- en NO3-concentraties in het ondiepe grond-
    water zijn opgenomen}

  { Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{.$define test}

uses
  ShareMem,
  windows, SysUtils, Classes, LargeArrays, ExtParU, USpeedProc, uDCfunc,UdsModel, UdsModelS,
  xyTable, uError, Dutils {$ifdef test} , fmx.Dialogs {$endif} ;

Const
  cModelID      = 104;  {-Key to this model (=unique-ID)}

  {-Mapping of dependent variable vector (=aantal te integreren snelheden)}
  cNrOfDepVar   = 1;    {-Length of dependent variable vector}
  cDVhardheid   = 1;    {-Hardheid (mmol/l)}

  {-Aantal keren dat een discontinuiteitsfunctie wordt aangeroepen in de procedure met
    snelheidsvergelijkingen (DerivsProc)}
  nDC = 0;
  
  {***** Used when booted for shell-usage ***}
  cnRP    = 4;  {-Nr. of RP-time-series that must be supplied by the shell in
                  EP[ indx-1 ].}
  cnSQ    = 0;  {-Nr. of point-time-series that must be supplied by the shell
                  in EP[ indx-1 ]. REM: point- sources niet aan de orde bij
                  stikstof-uitspoeling!}
  cnRQ    = 0;  {-Nr. of line-time-series that must be supplied
                  by the shell in EP[ indx-1 ]. REM: point- sources niet aan de
                  orde bij stikstof-uitspoeling!}

  {-Mapping of EP[cEP0]}
  cNrXIndepTblsInEP0 = 5;  {-Nr. of XIndep-tables in EP[cEP0]}
  cNrXdepTblsInEP0   = 0;  {-Nr. of Xdep-tables   in EP[cEP0]}

  cTbMinMaxValKeys  = 2;   {-EP[cEP0]: xIndep-Table numbering; 0&1 are reserved}
  cTBpCO2           = 3;
  cTbConst_a_b      = 4;

  {-Mapping of EP[cEP1]: xdep-Table numbering}
  cTbLandgebr       = 0;
  cTbBodemSrt       = 1;
  cTbSO4conc        = 2;
  cTbNO3conc        = 3;

  {-Landgebruik codes}
  cgras = 1; cbouwland = 2; cmais = 3; cnatuur = 4; cstedelijk = 5;

  {-Model specifieke fout-codes}
  cInvalidLandgebr = -9301;
  cInvalidBodemSrt = -9302;
  cInvalidSO4conc  = -9303;
  cInvalidNO3conc  = -9304;

var
  Indx: Integer; {-Index of Boot-procedure used. Must be set by boot-procedure!}
  {-Als verschillende TBootEPArray-functies de 'mapping' beinvloeden van
    de externe parameters op de EPArray, dan kan deze variabele binnen de
    Speed-procedures worden benut om toch de gewenste gegevens te vinden}
  ModelProfile: TModelProfile;
                 {-Object met met daarin de status van de discontinuiteitsfuncties
				   (zie nDC) }
  {-Min/max values of key-values: must be set by boot-procedure!}
  cLandgebrMin, cLandgebrMax,
  cBodemSrtMin, cBodemSrtMax: Integer;
  cSO4concMin, cSO4concMax,
  cNO3concMin, cNO3concMax: Double;

Procedure MyDllProc( Reason: Integer );
begin
  if Reason = DLL_PROCESS_DETACH then begin {-DLL is unloading}
    {-Cleanup code here}
	if ( nDC > 0 ) then
      ModelProfile.Free;
  end;
end;

Procedure DerivsProc( var x: Double; var y, dydx: TLargeRealArray;
                      var EP: TExtParArray; var Direction: TDirection;
                      var Context: Tcontext; var aModelProfile: PModelProfile; var IErr: Integer );
{-Returns the derivatives dydx at location x, given, x, the function values y
  and the external parameters EP. IErr=0 if no error occured during the
  calculation of dydx}
var
  LandGebr, BodemSrt: Integer; {key-values}
  SO4conc, NO3conc: Double;
  i: Integer;

Function SetKeyValues( var IErr: Integer ): Boolean;
  {Assign valid values for Gt, Landgebr, BodemSrt, SO4conc}
begin
  Result := False;
  with EP[ indx-1 ].xDep do begin {-Value of indx MUST be set by boot-procedure}
    Landgebr := Trunc( Items[ cTbLandgebr ].EstimateY( x, Direction ) );
    if ( Landgebr < cLandgebrMin ) or ( Landgebr > cLandgebrMax ) then begin
      IErr := cInvalidLandgebr; Exit;
    end;
    BodemSrt := Trunc( Items[ cTbBodemSrt ].EstimateY( x, Direction ) );
    if ( BodemSrt < cBodemSrtMin ) or ( BodemSrt > cBodemSrtMax ) then begin
      IErr := cInvalidBodemSrt; Exit;
    end;
    SO4conc := Items[ cTbSO4conc ].EstimateY( x, Direction );
    if ( SO4conc < cSO4concMin ) or ( SO4conc > cSO4concMax ) then begin
      IErr := cInvalidSO4conc; Exit;
    end;
    NO3conc := Items[ cTbNO3conc ].EstimateY( x, Direction );
    if ( NO3conc < cNO3concMin ) or ( NO3conc > cNO3concMax ) then begin
      IErr := cInvalidNO3conc; Exit;
    end;
  end;
  Result := True;
end;

Function Ca_natuurlijk( const LandGebr, BodemSrt: Integer ): Double;
  Function pCO2( const LandGebr, BodemSrt: Integer ): Double;
  begin
    with EP[ cEP0 ].xInDep.Items[ cTBpCO2 ] do
      Result := GetValue( LandGebr, BodemSrt );
  end;
begin
  Result := exp( 1.9 - 0.36 * pCO2( LandGebr, BodemSrt ) );
end;

Function Ca_antropogeen: Double;
  Function a: Double;
  begin
    with EP[ cEP0 ].xInDep.Items[ cTbConst_a_b ] do
      Result := GetValue( 1, 1 );
  end;
  Function b: Double;
  begin
    with EP[ cEP0 ].xInDep.Items[ cTbConst_a_b ] do
      Result := GetValue( 1, 2 );
  end;
begin
  Result := ( a * SO4conc ) + ( b * NO3conc );
end;

begin

  IErr := cUnknownError;
  for i := 1 to cNrOfDepVar do {-Default speed = 0}
    dydx[ i ] := 0;

  {-Geef de aanroepende procedure een handvat naar het ModelProfiel}
  if ( nDC > 0 ) then
    aModelProfile := @ModelProfile
  else
    aModelProfile := NIL;
    IErr := cNoError;

  if ( Context = UpdateYstart ) then begin
    {-Override initial values on ystart-vector here}
    {-Converteer dag-waarden uit tijdreeksen en invoertijdstippen afkomstig van
        de Shell naar jaren}
    if ( indx = cBoot2 ) then
      ScaleTimesFromShell( cFromDayToYear, EP );

  end else begin             {-Fill dydx-vector}
    {-Set Key-values}
    if not SetKeyValues( IErr ) then
      exit; {-Landgebr, BodemSrt, SO4conc, NO3conc}
    dydx[ cDVhardheid ] := Ca_natuurlijk( LandGebr, BodemSrt ) +
                           Ca_antropogeen;
  end;
end; {-DerivsProc}

Function DefaultBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-xDep-tables (Landgebruik, bodemsoort, SO4conc, NO3conc  are NOT set by this
    boot-procedure: they have to be initialised in another way}
Procedure SetMinMaxKeyValues;
  {Assign min/max values for Gt, Landgebr and BodemSrt}
begin
  with EP[ cEP0 ].xInDep.Items[ cTbMinMaxValKeys ] do begin
    cLandgebrMin := Trunc( GetValue( 1, 1 ) );
    cLandgebrMax := Trunc( GetValue( 1, 2 ) );
    cBodemSrtMin := Trunc( GetValue( 1, 3 ) );
    cBodemSrtMax := Trunc( GetValue( 1, 4 ) );
    cSO4concMin  :=        GetValue( 1, 5 );
    cSO4concMax  :=        GetValue( 1, 6 );
    cNO3concMin  :=        GetValue( 1, 7 );
    cNO3concMax  :=        GetValue( 1, 8 );
  end;
end;
Begin
  Result := DefaultBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cNrOfDepVar, nDC, cNrXIndepTblsInEP0,
                                       cNrXdepTblsInEP0, Indx, EP );
  if ( Result = cNoError ) then
    SetMinMaxKeyValues;
end;

Function TestBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Apart from the defaults for TestBootEP, this procedure also sets the
    xDep-tables (Landgebruik, bodemsoort, SO4conc, NO3conc), so the model is
    ready-to-run }
Begin
  Result := DefaultBootEP( EpDir, BootEpArrayOption, EP );
  if ( Result <> cNoError ) then exit;
  Result := DefaultTestBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cnRP + cnSQ + cnRQ, Indx,
                                           EP );
  if ( Result <> cNoError ) then exit;
  SetReadyToRun( EP);
end;

Function BootEPForShell( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-xDep-tables (Landgebruik, bodemsoort, SO4conc, NO3conc) are NOT set by this
    boot-procedure: they must be supplied by the shell }
begin
  {$ifdef test} Showmessage( 'BootEPForShell model 104' ) {$endif};
  Result := DefaultBootEP( EpDir, cBootEPFromTextFile, EP );
  if ( Result = cNoError ) then
    Result := DefaultBootEPForShell( cnRP, cnSQ, cnRQ, Indx, EP );
end;

Exports DerivsProc       index cModelIndxForTDSmodels, {999}
        DefaultBootEP    index cBoot0, {1}
        TestBootEP       index cBoot1, {2}
        BootEPForShell   index cBoot2; {3}
begin
  {-This 'DLL-Main-block' is executed  when the DLL is initially loaded into
    memory (Reason = DLL_PROCESS_ATTACH)}
  DLLProc := @MyDllProc;
  Indx := cBootEPArrayVariantIndexUnknown;
  if ( nDC > 0 ) then
    ModelProfile := TModelProfile.Create( nDC );
end.
