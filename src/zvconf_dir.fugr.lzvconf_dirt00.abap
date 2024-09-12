*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZVCONF_DIR......................................*
TABLES: ZVCONF_DIR, *ZVCONF_DIR. "view work areas
CONTROLS: TCTRL_ZVCONF_DIR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZVCONF_DIR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVCONF_DIR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVCONF_DIR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVCONF_DIR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVCONF_DIR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVCONF_DIR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVCONF_DIR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVCONF_DIR_TOTAL.

*.........table declarations:.................................*
TABLES: ZCONF_DIR                      .
