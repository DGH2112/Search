(**
  
  This module contains constants tha tare used through the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Jun 2018
  
**)
Unit Search.Constants;

Interface

Uses
  Search.Types, 
  Vcl.Graphics;

Const
  (** A constant array of output colour names. **)
  strSearchColour : Array[Low(TSearchColour)..High(TSearchColour)] Of String = (
    'None',
    'Title',
    'Header',
    'Footer',
    'SearchPath',
    'FoundSearchPath',
    'FileInfo',
    'RegExLineNumbers',
    'RegExLineOutput',
    'RegExFindOutputFG',
    'RegExFindOutputBG',
    'SummaryOutput',
    'ZipFile',
    'Success',
    'Warning',
    'Exception',
    'HelpHeader',
    'HelpInfo',
    'HelpText',
    'HelpSwitch',
    'HelpFootNote',
    'Input'
  );
  (** A constant array of colour names for the console output colours. **)
  strSearchColourList : Array[Low(TSearchColourList)..High(TSearchColourList)] Of String = (
    'Black',
    'Maroon',
    'Green',
    'Navy',
    'Olive', 
    'Purple', 
    'Teal',
    'Gray',
    'Red',
    'Lime', 
    'Blue', 
    'Yellow', 
    'Fuchsia',
    'Aqua', 
    'White', 
    'None'
  );
  (** A constant array of colour for the console output colours. **)
  SearchColourList : Array[Low(TSearchColourList)..High(TSearchColourList)] Of TColor = (
    clBlack,
    clMaroon,
    clGreen,
    clNavy,
    clOlive, 
    clPurple, 
    clTeal,
    clGray,
    clRed,
    clLime, 
    clBlue, 
    clYellow, 
    clFuchsia,
    clAqua, 
    clWhite, 
    clNone
  );

  (** An output format for all dates. **)
  strOutputDateFmt = 'ddd dd/mmm/yyyy hh:mm:ss';

Implementation

End.
