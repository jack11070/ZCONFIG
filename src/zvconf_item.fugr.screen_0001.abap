PROCESS BEFORE OUTPUT.
 MODULE LISTE_INITIALISIEREN.
 LOOP AT EXTRACT WITH CONTROL
  TCTRL_ZVCONF_ITEM CURSOR NEXTLINE.
   MODULE LISTE_SHOW_LISTE.
 ENDLOOP.
 MODULE FILL_SUBSTFLDS.
*
PROCESS AFTER INPUT.
 MODULE LISTE_EXIT_COMMAND AT EXIT-COMMAND.
 MODULE LISTE_BEFORE_LOOP.
 LOOP AT EXTRACT.
   MODULE LISTE_INIT_WORKAREA.
   CHAIN.
    FIELD ZVCONF_ITEM-ITEM_NO .
    FIELD ZVCONF_ITEM-ITEM_TEXT .
    FIELD ZVCONF_ITEM-TABNAME .
    FIELD ZVCONF_ITEM-ZTYPE .
    FIELD ZVCONF_ITEM-PROGNAME .
    FIELD ZVCONF_ITEM-FORMNAME .
    MODULE SET_UPDATE_FLAG ON CHAIN-REQUEST.
   ENDCHAIN.
   FIELD VIM_MARKED MODULE LISTE_MARK_CHECKBOX.
   CHAIN.
    FIELD ZVCONF_ITEM-ITEM_NO .
    MODULE LISTE_UPDATE_LISTE.
   ENDCHAIN.
 ENDLOOP.
 MODULE LISTE_AFTER_LOOP.