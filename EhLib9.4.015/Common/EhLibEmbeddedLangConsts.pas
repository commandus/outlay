{*******************************************************}
{                                                       }
{                      EhLib 9.4                        }
{               Embedded Language Resources             }
{                     Build 9.4.001                     }
{                                                       }
{     Copyright (c) 2017-2019 by Dmitry V. Bolshakov    }
{                                                       }
{*******************************************************}

unit EhLibEmbeddedLangConsts;

{$I EhLib.Inc}

interface

implementation

{$IFDEF EH_LIB_12} 
  {$R EhLibLangConsts.BGR.dfm}
  {$R EhLibLangConsts.CHS.dfm}
  {$R EhLibLangConsts.CSY.dfm}
  {$R EhLibLangConsts.DEU.dfm}
  {$R EhLibLangConsts.ENU.dfm}
  {$R EhLibLangConsts.ESP.dfm}
  {$R EhLibLangConsts.FAR.dfm}
  {$R EhLibLangConsts.FRA.dfm}
  {$R EhLibLangConsts.JPN.dfm}
  {$R EhLibLangConsts.KOR.dfm}
  {$R EhLibLangConsts.PLK.dfm}
  {$R EhLibLangConsts.PTB.dfm}
  {$R EhLibLangConsts.RUS.dfm}
  {$R EhLibLangConsts.SKY.dfm}
  {$R EhLibLangConsts.TRK.dfm}
  {$R EhLibLangConsts.UKR.dfm}
{$ELSE}            
  {$R EhLibLangConsts.ENU.dfm}
  {$R EhLibLangConsts.FRA.dfm}
  {$R EhLibLangConsts.RUS.dfm}
  {$R EhLibLangConsts.UKR.dfm}
{$ENDIF}

end.
