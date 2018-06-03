(**
  
  This module contains a record which describes all the options that can be applied to the search.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Jun 2018
  
**)
Unit Search.Options;

Interface

Uses
  Search.Types, 
  System.Classes;

Type
  (** A record to describe all the options which can be applied to the searches. **)
  TSearchOptions = Record
  Strict Private
  Public
    FUSize: Int64;                // This is the upper limit of a file size search
    FLSize: Int64;                // This is the lower limit of a file size search
    FLDate: TDateTime;            // This is the lower limit of a file date search
    FUDate: TDateTime;            // This is the upper limit of a file date search
    FFileAttrs: TSearchFileAttrs; // This is a list of file attributes that are required in a search
    FSummaryLevel: Integer;       // This is the level of directory of summarisation required
    FOrderFilesBy: TOrderBy;      // A type to indicate the order of file sorting
    FOrderFilesDirection: TOrderDirection; // A type to indicate the direction of sorting of files
    FRegExSearch: String;         // A string for which should be searched for inside text files
    FRegExSurroundingLines : Integer; // A variable to hold the type of date to display and search for
    FDateType: TDateType;         // A file name which hold exclusions which need to be applied to the searches
    FExlFileName: String;
    FExclusions: TStringList;     // A string list of exclusions to be applied to the searches
    FOwnerSearch : Boolean;
    FOwnerRegExSearch : String;
    FSizeFormat: TSizeFormat;     // A string to store the loading and saving settings filename in
  End;

Implementation

End.
