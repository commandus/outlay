{*******************************************************}
{                                                       }
{                        EhLib 9.4                      }
{                                                       }
{        Classes for detection Images stream format     }
{                      Build 9.4.01                     }
{                                                       }
{   Copyright (c) 2011-2019 by Dmitry V. Bolshakov      }
{                                                       }
{*******************************************************}

unit EhLibImages;

{$I EhLib.Inc}

interface

uses ToolCtrlsEh, EhLibJPegImage
{$IFDEF EH_LIB_11} ,EhLibGIFImage  {$ENDIF} { Borland Developer Studio 2007 }
{$IFDEF EH_LIB_12} ,EhLibPNGImage {$ENDIF} { CodeGear RAD Studio 2009 }
  ;

implementation

initialization
end.
