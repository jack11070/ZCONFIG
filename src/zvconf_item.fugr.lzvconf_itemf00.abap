*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: ZVCONF_ITEM.....................................*
FORM GET_DATA_ZVCONF_ITEM.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZCONF_ITEM WHERE
(VIM_WHERETAB) .
    CLEAR ZVCONF_ITEM .
ZVCONF_ITEM-MANDT =
ZCONF_ITEM-MANDT .
ZVCONF_ITEM-ZMODULE =
ZCONF_ITEM-ZMODULE .
ZVCONF_ITEM-DIR_NO =
ZCONF_ITEM-DIR_NO .
ZVCONF_ITEM-ITEM_NO =
ZCONF_ITEM-ITEM_NO .
ZVCONF_ITEM-ITEM_TEXT =
ZCONF_ITEM-ITEM_TEXT .
ZVCONF_ITEM-TABNAME =
ZCONF_ITEM-TABNAME .
ZVCONF_ITEM-ZTYPE =
ZCONF_ITEM-ZTYPE .
ZVCONF_ITEM-PROGNAME =
ZCONF_ITEM-PROGNAME .
ZVCONF_ITEM-FORMNAME =
ZCONF_ITEM-FORMNAME .
    SELECT SINGLE * FROM ZCONF_DIR WHERE
ZMODULE = ZCONF_ITEM-ZMODULE AND
DIR_NO = ZCONF_ITEM-DIR_NO .
    IF SY-SUBRC EQ 0.
ZVCONF_ITEM-DIR_TEXT =
ZCONF_DIR-DIR_TEXT .
    ENDIF.
<VIM_TOTAL_STRUC> = ZVCONF_ITEM.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZVCONF_ITEM .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZVCONF_ITEM.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZVCONF_ITEM-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZCONF_ITEM WHERE
  ZMODULE = ZVCONF_ITEM-ZMODULE AND
  DIR_NO = ZVCONF_ITEM-DIR_NO AND
  ITEM_NO = ZVCONF_ITEM-ITEM_NO .
    IF SY-SUBRC = 0.
    DELETE ZCONF_ITEM .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZCONF_ITEM WHERE
  ZMODULE = ZVCONF_ITEM-ZMODULE AND
  DIR_NO = ZVCONF_ITEM-DIR_NO AND
  ITEM_NO = ZVCONF_ITEM-ITEM_NO .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZCONF_ITEM.
    ENDIF.
ZCONF_ITEM-MANDT =
ZVCONF_ITEM-MANDT .
ZCONF_ITEM-ZMODULE =
ZVCONF_ITEM-ZMODULE .
ZCONF_ITEM-DIR_NO =
ZVCONF_ITEM-DIR_NO .
ZCONF_ITEM-ITEM_NO =
ZVCONF_ITEM-ITEM_NO .
ZCONF_ITEM-ITEM_TEXT =
ZVCONF_ITEM-ITEM_TEXT .
ZCONF_ITEM-TABNAME =
ZVCONF_ITEM-TABNAME .
ZCONF_ITEM-ZTYPE =
ZVCONF_ITEM-ZTYPE .
ZCONF_ITEM-PROGNAME =
ZVCONF_ITEM-PROGNAME .
ZCONF_ITEM-FORMNAME =
ZVCONF_ITEM-FORMNAME .
    IF SY-SUBRC = 0.
    UPDATE ZCONF_ITEM ##WARN_OK.
    ELSE.
    INSERT ZCONF_ITEM .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZVCONF_ITEM-UPD_FLAG,
STATUS_ZVCONF_ITEM-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZVCONF_ITEM.
  SELECT SINGLE * FROM ZCONF_ITEM WHERE
ZMODULE = ZVCONF_ITEM-ZMODULE AND
DIR_NO = ZVCONF_ITEM-DIR_NO AND
ITEM_NO = ZVCONF_ITEM-ITEM_NO .
ZVCONF_ITEM-MANDT =
ZCONF_ITEM-MANDT .
ZVCONF_ITEM-ZMODULE =
ZCONF_ITEM-ZMODULE .
ZVCONF_ITEM-DIR_NO =
ZCONF_ITEM-DIR_NO .
ZVCONF_ITEM-ITEM_NO =
ZCONF_ITEM-ITEM_NO .
ZVCONF_ITEM-ITEM_TEXT =
ZCONF_ITEM-ITEM_TEXT .
ZVCONF_ITEM-TABNAME =
ZCONF_ITEM-TABNAME .
ZVCONF_ITEM-ZTYPE =
ZCONF_ITEM-ZTYPE .
ZVCONF_ITEM-PROGNAME =
ZCONF_ITEM-PROGNAME .
ZVCONF_ITEM-FORMNAME =
ZCONF_ITEM-FORMNAME .
    SELECT SINGLE * FROM ZCONF_DIR WHERE
ZMODULE = ZCONF_ITEM-ZMODULE AND
DIR_NO = ZCONF_ITEM-DIR_NO .
    IF SY-SUBRC EQ 0.
ZVCONF_ITEM-DIR_TEXT =
ZCONF_DIR-DIR_TEXT .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZVCONF_ITEM-DIR_TEXT .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZVCONF_ITEM USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZVCONF_ITEM-ZMODULE TO
ZCONF_ITEM-ZMODULE .
MOVE ZVCONF_ITEM-DIR_NO TO
ZCONF_ITEM-DIR_NO .
MOVE ZVCONF_ITEM-ITEM_NO TO
ZCONF_ITEM-ITEM_NO .
MOVE ZVCONF_ITEM-MANDT TO
ZCONF_ITEM-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZCONF_ITEM'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZCONF_ITEM TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZCONF_ITEM'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZVCONF_ITEM USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZCONF_ITEM-MANDT =
ZVCONF_ITEM-MANDT .
ZCONF_ITEM-ZMODULE =
ZVCONF_ITEM-ZMODULE .
ZCONF_ITEM-DIR_NO =
ZVCONF_ITEM-DIR_NO .
ZCONF_ITEM-ITEM_NO =
ZVCONF_ITEM-ITEM_NO .
ZCONF_ITEM-ITEM_TEXT =
ZVCONF_ITEM-ITEM_TEXT .
ZCONF_ITEM-TABNAME =
ZVCONF_ITEM-TABNAME .
ZCONF_ITEM-ZTYPE =
ZVCONF_ITEM-ZTYPE .
ZCONF_ITEM-PROGNAME =
ZVCONF_ITEM-PROGNAME .
ZCONF_ITEM-FORMNAME =
ZVCONF_ITEM-FORMNAME .
    SELECT SINGLE * FROM ZCONF_DIR WHERE
ZMODULE = ZCONF_ITEM-ZMODULE AND
DIR_NO = ZCONF_ITEM-DIR_NO .
    IF SY-SUBRC EQ 0.
ZVCONF_ITEM-DIR_TEXT =
ZCONF_DIR-DIR_TEXT .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZVCONF_ITEM-DIR_TEXT .
    ENDIF.
ENDFORM.
