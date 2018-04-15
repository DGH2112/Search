(**
  
  This module contains interfaces for use throughout the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    15 Apr 2018
  
**)
Unit Search.Interfaces;

Interface

Uses
  System.RegularExpressions, 
  System.Generics.Collections, 
  VCL.Graphics,
  Search.RegExMatches, 
  Search.Types;

Type
  (** An interface for file attributes. **)
  ISearchFile = Interface
  ['{A9EB7CB7-F77E-4270-941E-12FD6CD5810C}']
    Function  GetDate : TDateTime;
    Function  GetSize : Int64;
    Function  GetCompressedSize : Int64;
    Function  GetAttributes : TSearchFileAttrs;
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
      A property to read and write the files size.
      @precon  None.
      @postcon None.
      @return  an Int64
    **)
    Property CompressedSize : Int64 Read GetCompressedSize;
    (**
      A property to read and write the files Attributes.
      @precon  None.
      @postcon None.
      @return  a TSearchFileAttrs
    **)
    Property Attributes : TSearchFileAttrs Read GetAttributes;
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

  (** An interface to describe a list of files found in the search. **)
  ISearchFiles = Interface
  ['{9C709996-ED4C-413A-948B-66D2562193FD}']
    Function  GetFile(Const iIndex : Integer) : ISearchFile;
    Function  GetCount : Integer;
    Function  GetPath : String;
    Function  GetHasCompressed : Boolean;
    Procedure SetPath(Const strPath : String);
    Function  Add(Const dtDate : TDateTime; Const iSize, iCompressedSize : Int64;
      Const setAttrs : TSearchFileAttrs; Const strOwner, strName, strSearchText : String) : Boolean;
      Overload;
    Function  OwnerWidth : Integer;
    Procedure OrderBy(Const OrderBy : TOrderBy; Const OrderDirection : TOrderDirection);
    Function  Add(Const FileInfo : ISearchFile) : Boolean; Overload;
    (**
      A property to return a specific file from the collection.
      @precon  iIndex must be a valid index.
      @postcon The file identified any the index is returned.
      @param   iIndex as an Integer as a Constant
      @return  an ISearchFile
    **)
    Property FileInfo[Const iIndex : Integer] : ISearchFile Read GetFile;
    (**
      A property to return the number of files in the collection.
      @precon  None.
      @postcon Returns the number of files in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      A property to read and write a path for the collection of files.
      @precon  None.
      @postcon Gets and sets the path of the file collection.
      @return  a String
    **)
    Property Path : String Read GetPath Write SetPath;
    (**
      This propperty gets whether the file collection has any compressed files in it.
      @precon  None.
      @postcon Returns whether the collection has an compressed files.
      @return  a Boolean
    **)
    Property HasCompressed : Boolean Read GetHasCompressed;
  End;

  (** An interface for the main search engine. **)
  ISearchEngine = Interface
  ['{678751F5-EF72-4159-B0A6-9097D790D3E0}']
    Function  GetExceptionColour : TColor;
    Function  GetStdHnd : THandle;
    Function  GetErrHnd : THandle;
    Procedure Run;
    (**
     This property returns the Exception colour to outside the class.
     @precon  None.
     @postcon Returns the Exception colour to outside the class.
     @return  a TColor
     **)
    Property ExceptionColour: TColor Read GetExceptionColour;
    (**
      This property returns the error handle for console output.
      @precon  None.
      @postcon Returns the error handle for console output.
      @return  a THandle
    **)
    Property ErrHnd : THandle Read GetErrHnd;
    (**
      This property returns the standard handle for console output.
      @precon  None.
      @postcon Returns the standard handle for console output.
      @return  a THandle
    **)
    Property StdHnd : THandle Read GetStdHnd;
  End;
  
Implementation

End.
