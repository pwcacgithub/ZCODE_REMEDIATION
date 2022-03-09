*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 17.01.2022 at 09:00:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTCODE_REM_DET..................................*
DATA:  BEGIN OF STATUS_ZTCODE_REM_DET                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCODE_REM_DET                .
CONTROLS: TCTRL_ZTCODE_REM_DET
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCODE_REM_DET                .
TABLES: ZTCODE_REM_DET                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
