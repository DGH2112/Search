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
  Search.RegExMatches, 
  Search.Types;

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

  (** An interface to describe a list of files found in the search. **)
  ISearchFiles = Interface
  ['{9C709996-ED4C-413A-948B-66D2562193FD}']
    Function  GetFile(Const iIndex : Integer) : ISearchFile;
    Function  GetCount : Integer;
    Function  GetPath : String;
    Procedure SetPath(Const strPath : String);
    Function  Add(Const dtDate : TDateTime; Const iSize : Int64; Const strAttr, strOwner,
      strName, strSearchText : String; Const iFileAttrs : Integer) : Boolean; Overload;
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
  End;
  
Implementation

End.
