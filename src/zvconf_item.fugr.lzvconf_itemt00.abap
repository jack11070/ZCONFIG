*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZVCONF_ITEM.....................................*
TABLES: ZVCONF_ITEM, *ZVCONF_ITEM. "view work areas
CONTROLS: TCTRL_ZVCONF_ITEM
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZVCONF_ITEM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVCONF_ITEM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVCONF_ITEM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVCONF_ITEM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVCONF_ITEM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVCONF_ITEM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVCONF_ITEM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVCONF_ITEM_TOTAL.

*.........table declarations:.................................*
TABLES: ZCONF_DIR                      .
TABLES: ZCONF_ITEM                     .
