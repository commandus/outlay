{*******************************************************}
{                                                       }
{                       EhLib 9.4                       }
{                    EhLibSplash form                   }
{                      Build 9.4.001                    }
{                                                       }
{    Copyright (c) 2013-2019 by Dmitry V. Bolshakov     }
{                                                       }
{*******************************************************}

unit EhLibSplash;

{$I EhLib.Inc}

interface

implementation

{$R EhLibSplash.res}

uses Classes, DesignIntf, ToolsAPI, Windows, TypInfo;

{$I EhlibVerInfo.Inc}
{$I EhLibEditionInfo.Inc}

procedure Init;
begin
{$IFDEF EH_LIB_9}

  SplashScreenServices.AddPluginBitmap(EhLibVerInfo + ' ' + EhLibBuildInfo,
    LoadBitmap(FindResourceHInstance(HInstance), 'EHLIB_SPLASH_ICON_24'),
{$IFDEF eval}
   True, EhLibEditionInfo
{$ELSE}
   False, EhLibEditionInfo
{$ENDIF}
);

{$ENDIF}
end;

initialization
  Init;
finalization
end.
