(**
  
  This module contains interfaces for use throughout the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    07 Apr 2018
  
**)
Unit Search.Interfaces;

Interface

Uses
  System.RegularExpressions, 
  System.Generics.Collections, 
  Search.RegExMatches;

Type
  (** An interface for file attributes. **)
  ISearchFile = Interface
  ['{A9EB7CB7-F77E-4270-941E-12FD6CD5810C}']
    Function  GetDate : TDateTime;
    Function  GetSize : Int64;
    Function  GetAttr : String;
    Function  GetOwner : String;
    Function  GetName : String;
    Function  GetRegExMatches : TList<TRegExMatches>;
    Procedure AddRegExLine(Const iLine : Integer; Const Matches : TMatchCollection);
    Function Clone : ISearchFile;
    (**
      A property to read and write the files date and time.
      @precon  None.
      @postcon None.
      @return  a TDateTime
    **)
    Property Date : TDateTime Read GetDate;
    (**
      A property to read and write the files size.
      @precon  None.
      @postcon None.
      @return  an Int64
    **)
    Property Size : Int64 Read GetSize;
    (**
      A property to read and write the files Attributes.
      @precon  None.
      @postcon None.
      @return  a String
    **)
    Property Attr : String Read GetAttr;
    (**
      A property to read and write the files owner.
      @precon  None.
      @postcon None.
      @return  a String
    **)
    Property Owner : String Read GetOwner;
    (**
      A property to read and write the files name.
      @precon  None.
      @postcon None.
      @return  a String
    **)
    Property FileName : String Read GetName;
    (**
      This property returns the regular expression matches for the file.
      @precon  None.
      @postcon Returns the regular expression matches for the file.
      @return  a TList<TRegExMatches>
    **)
    Property RegExMatches : TList<TRegExMatches> Read GetRegExMatches;
  End;

Implementation

End.
