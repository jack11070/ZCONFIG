************************************************************************
* Program Name      : ZCONFIG
* Descriptions      : 提供各模組自定義設定的快速路徑
* Package           : ZCONFIG
* Reference Tables  : ZCONF_DIR, ZCONF_ITEM
************************************************************************
REPORT  ZCUSTOM_CONFIG.
************************************************************************
* Class Definitions
************************************************************************
CLASS LCL_TREE_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      HANDLE_TREE_LINK_CLICK FOR EVENT LINK_CLICK OF CL_GUI_COLUMN_TREE IMPORTING NODE_KEY ITEM_NAME.
ENDCLASS.
************************************************************************
* Types Definitions
************************************************************************
TYPES:BEGIN OF TY_ITEM,
        DIR_NO    TYPE ZCONF_DIR-DIR_NO,
        DIR_TEXT  TYPE ZCONF_DIR-DIR_TEXT,
        ITEM_NO   TYPE ZCONF_ITEM-ITEM_NO,
        ITEM_TEXT TYPE ZCONF_ITEM-ITEM_TEXT,
        TABNAME   TYPE ZCONF_ITEM-TABNAME,
        ZTYPE     TYPE ZCONF_ITEM-ZTYPE,
        PROGNAME  TYPE ZCONF_ITEM-PROGNAME,
        FORMNAME  TYPE ZCONF_ITEM-FORMNAME,
      END OF TY_ITEM.
************************************************************************
* Constants Definitions
************************************************************************
CONSTANTS:
  GC_ENH_CONF    TYPE STRING VALUE 'ENH_CONF',
  GC_CUS_CONF    TYPE STRING VALUE 'CUS_CONF',
  GC_COLUMN_BTN  TYPE TV_ITMNAME VALUE 'BUTTON',
  GC_COLUMN_ITEM TYPE TV_ITMNAME VALUE 'ITEM'.
************************************************************************
* Data Definitions
************************************************************************
DATA: G_MODULE         TYPE STRING,
      GT_CONFIG        TYPE SORTED TABLE OF TY_ITEM WITH UNIQUE KEY DIR_NO ITEM_NO,
      GO_DOCKING_TREE  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GO_TREE          TYPE REF TO CL_GUI_COLUMN_TREE,
      GO_EVENT_TREE    TYPE REF TO LCL_TREE_EVENT_RECEIVER,
      GT_SEL_CONDITION TYPE TABLE OF VIMSELLIST WITH HEADER LINE.
************************************************************************
* Class Implementation
************************************************************************
CLASS LCL_TREE_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_TREE_LINK_CLICK.
    PERFORM HANDLE_TREE_LINK_CLICK USING NODE_KEY ITEM_NAME.
  ENDMETHOD.
ENDCLASS.
************************************************************************
*  Selection Screen
************************************************************************
SELECTION-SCREEN PUSHBUTTON 1(20) REFRESH USER-COMMAND REFRESH.
************************************************************************
*  Initialization
************************************************************************
INITIALIZATION.
  PERFORM INI_DATA.
  PERFORM GET_DATA.
************************************************************************
*  At Selection Screen Output
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM EXCLUDE_STD_GUI_STATUS.
  PERFORM CREATE_TREE.
  PERFORM DISPLAY_DATA.
************************************************************************
*  At Selection Screen
************************************************************************
AT SELECTION-SCREEN.
  IF SY-UCOMM = 'REFRESH'.
    PERFORM GET_DATA.
    PERFORM DISPLAY_DATA.
  ENDIF.
*&---------------------------------------------------------------------*
*& Form Get_data
*&---------------------------------------------------------------------*
*& 透過自定義設定目錄與設定項目表格取得資料
*&---------------------------------------------------------------------*
FORM GET_DATA.
  SELECT
    FROM ZCONF_DIR AS DIR
    LEFT JOIN ZCONF_ITEM AS ITEM
           ON ITEM~ZMODULE = DIR~ZMODULE
          AND ITEM~DIR_NO  = DIR~DIR_NO
    FIELDS DIR~DIR_NO  , DIR~DIR_TEXT, ITEM~ITEM_NO , ITEM~ITEM_TEXT,
           ITEM~TABNAME, ITEM~ZTYPE  , ITEM~PROGNAME, ITEM~FORMNAME
  WHERE DIR~ZMODULE = @G_MODULE
  INTO TABLE @GT_CONFIG.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Create_tree
*&---------------------------------------------------------------------*
*& 建立樹狀結構，透過類別 CL_GUI_DOCKING_CONTAINER 建立
*&---------------------------------------------------------------------*
FORM CREATE_TREE.
  DATA: LS_HEADER TYPE TREEV_HHDR,
        LT_EVENTS TYPE CNTL_SIMPLE_EVENTS.
  CHECK GO_DOCKING_TREE IS NOT BOUND.
  "劃立樹狀結構範圍
  CREATE OBJECT GO_DOCKING_TREE
    EXPORTING
      RATIO = 93
      SIDE  = CL_GUI_DOCKING_CONTAINER=>DOCK_AT_BOTTOM.
  "建立樹狀結構
  LS_HEADER-HEADING = |{ G_MODULE }自定義設定|.
  LS_HEADER-WIDTH = 50.
  CREATE OBJECT GO_TREE
    EXPORTING
      PARENT                = GO_DOCKING_TREE
      NODE_SELECTION_MODE   = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION        = ABAP_TRUE
      HIERARCHY_COLUMN_NAME = GC_COLUMN_ITEM
      HIERARCHY_HEADER      = LS_HEADER.
  "取得事件並加入事件
  GO_TREE->GET_REGISTERED_EVENTS( IMPORTING EVENTS = LT_EVENTS ).
  APPEND VALUE #( EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_LINK_CLICK APPL_EVENT = 'X' ) TO LT_EVENTS.
  GO_TREE->SET_REGISTERED_EVENTS( EVENTS = LT_EVENTS ).
  "建立事件處理類別
  CREATE OBJECT GO_EVENT_TREE.
  SET HANDLER GO_EVENT_TREE->HANDLE_TREE_LINK_CLICK FOR GO_TREE.
  "在樹狀節點中加入按鈕
  GO_TREE->INSERT_HIERARCHY_COLUMN( NAME =  GC_COLUMN_BTN ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Display_Data
*&---------------------------------------------------------------------*
*& 顯示資料
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  DATA: LT_NODES    TYPE TREEV_NTAB,
        LT_ITEMS    TYPE STANDARD TABLE OF STREEITM WITH DEFAULT KEY,
        LV_NODE_KEY TYPE TV_NODEKEY.
  DEFINE MACRO_APPEND_NODE.
    IF &2 IS INITIAL.
      APPEND VALUE #( node_key  = &1
                      n_image   = &3
                      isfolder  = &4 ) TO lt_nodes.
    ELSE.
      APPEND VALUE #( node_key  = &1
                      relatkey  = &2
                      relatship = cl_gui_column_tree=>relat_last_child
                      n_image   = &3
                      isfolder  = &4 ) TO lt_nodes.
    ENDIF.
  END-OF-DEFINITION.
  DEFINE MACRO_APPEND_BTN.
    APPEND VALUE #( node_key  = &1
                    item_name = gc_column_btn
                    class     = cl_gui_column_tree=>item_class_link
                    t_image   = icon_execute_object ) TO lt_items.
  END-OF-DEFINITION.
  DEFINE MACRO_APPEND_ITEM.
    APPEND VALUE #( node_key  = &1
                    item_name = gc_column_item
                    text      = &2
                    disabled  = ABAP_FALSE ) TO lt_items.
  END-OF-DEFINITION.
  GO_TREE->DELETE_ALL_NODES( ).
  "新增程式「固定」項目
  MACRO_APPEND_NODE GC_CUS_CONF '' ICON_SPACE ''.
  MACRO_APPEND_BTN  GC_CUS_CONF.
  MACRO_APPEND_ITEM GC_CUS_CONF '管理自定義設定'.
  "新增按「交易代碼」項目
  LOOP AT GT_CONFIG ASSIGNING FIELD-SYMBOL(<FS_ITEM>).
    AT NEW DIR_NO.
      MACRO_APPEND_NODE <FS_ITEM>-DIR_NO '' '' 'X'.
      MACRO_APPEND_ITEM <FS_ITEM>-DIR_NO <FS_ITEM>-DIR_TEXT.
    ENDAT.
    IF <FS_ITEM>-ITEM_NO IS NOT INITIAL OR <FS_ITEM>-ITEM_TEXT IS NOT INITIAL.
      LV_NODE_KEY = |{ <FS_ITEM>-DIR_NO }-{ <FS_ITEM>-ITEM_NO }|.
      MACRO_APPEND_NODE LV_NODE_KEY <FS_ITEM>-DIR_NO ICON_SPACE ''.
      MACRO_APPEND_BTN  LV_NODE_KEY.
      MACRO_APPEND_ITEM LV_NODE_KEY <FS_ITEM>-ITEM_TEXT.
    ENDIF.
  ENDLOOP.
  CALL METHOD GO_TREE->ADD_NODES_AND_ITEMS
    EXPORTING
      NODE_TABLE                = LT_NODES
      ITEM_TABLE                = LT_ITEMS
      ITEM_TABLE_STRUCTURE_NAME = 'STREEITM'
    EXCEPTIONS
      OTHERS                    = 1.
  GO_TREE->EXPAND_ROOT_NODES( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Handle_Tree_Link_Click
*&---------------------------------------------------------------------*
*& 處理樹狀資料點擊事件
*&---------------------------------------------------------------------*
FORM HANDLE_TREE_LINK_CLICK  USING  P_NODE_KEY TYPE LVC_NKEY
                                  P_ITEM_NAME TYPE TV_ITMNAME.
  CASE P_NODE_KEY.
    WHEN GC_CUS_CONF.
      REFRESH GT_SEL_CONDITION.
      APPEND VALUE #( VIEWFIELD = 'ZMODULE' OPERATOR = 'EQ' VALUE = G_MODULE ) TO GT_SEL_CONDITION.
      PERFORM CALL_SM34 USING 'ZCONF'.
      PERFORM GET_DATA.
      PERFORM DISPLAY_DATA.
    WHEN OTHERS.
      CHECK STRLEN( P_NODE_KEY ) > 3.
      SPLIT P_NODE_KEY AT '-' INTO DATA(LV_DIR_NO) DATA(LV_ITEM_NO).
      READ TABLE GT_CONFIG WITH KEY DIR_NO = LV_DIR_NO ITEM_NO = LV_ITEM_NO ASSIGNING FIELD-SYMBOL(<FS_ITEM>).
      IF <FS_ITEM>-ZTYPE = 'SM30'.
        PERFORM CALL_SM30 USING <FS_ITEM>-TABNAME.
      ELSEIF <FS_ITEM>-ZTYPE = 'SM34'.
        PERFORM CALL_SM34 USING <FS_ITEM>-TABNAME.
      ELSE.
        PERFORM (<FS_ITEM>-FORMNAME) IN PROGRAM (<FS_ITEM>-PROGNAME) IF FOUND.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Call_SM30
*&---------------------------------------------------------------------*
*& 呼叫 SM30 編輯模式（U：編輯｜S：顯示）
*&---------------------------------------------------------------------*
FORM CALL_SM30 USING P_TABLENAME TYPE DD02V-TABNAME.
  DATA: L_ACTION TYPE C VALUE 'U'.
  IF GT_SEL_CONDITION[] IS INITIAL.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        ACTION    = L_ACTION
        VIEW_NAME = P_TABLENAME.
  ELSE.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        ACTION      = L_ACTION
        VIEW_NAME   = P_TABLENAME
      TABLES
        DBA_SELLIST = GT_SEL_CONDITION[].
    REFRESH: GT_SEL_CONDITION.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Call_SM34
*&---------------------------------------------------------------------*
*& 呼叫 SM34 編輯模式（U：編輯｜S：顯示）
*&---------------------------------------------------------------------*
FORM CALL_SM34 USING P_CLUSTERNAME TYPE VCLDIR-VCLNAME.
  DATA: L_ACTION TYPE C VALUE 'U'.
  IF GT_SEL_CONDITION[] IS INITIAL.
    CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
      EXPORTING
        MAINTENANCE_ACTION = 'U'
        VIEWCLUSTER_NAME   = P_CLUSTERNAME.
  ELSE.
    CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
      EXPORTING
        MAINTENANCE_ACTION = 'U'
        VIEWCLUSTER_NAME   = P_CLUSTERNAME
      TABLES
        DBA_SELLIST        = GT_SEL_CONDITION[].
    REFRESH: GT_SEL_CONDITION.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCLUDE_STD_GUI_STATUS
*&---------------------------------------------------------------------*
*& 清除標準 GUI 狀態：SPOS（儲存）、ONLI（執行）
*&---------------------------------------------------------------------*
FORM EXCLUDE_STD_GUI_STATUS.
  DATA:
  LT_EXCLUDE  TYPE TABLE OF SY-UCOMM.
  LT_EXCLUDE = VALUE #( ( 'ONLI' ) ( 'SPOS' ) ).
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      P_STATUS  = SY-PFKEY
      P_PROGRAM = SY-CPROG
    TABLES
      P_EXCLUDE = LT_EXCLUDE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INI_DATA
*&---------------------------------------------------------------------*
*& 從交易代碼初始化資料
*&---------------------------------------------------------------------*
FORM INI_DATA.
  REFRESH = `頁面刷新`.
  DATA(LV_TCODE) = SY-TCODE.
  REPLACE FIRST OCCURRENCE OF `CONFIG` IN LV_TCODE WITH ''.
  G_MODULE = LV_TCODE.
  SELECT SINGLE COUNT( * )
    FROM DD07L
   WHERE DOMNAME    = 'ZMODULE'
     AND AS4LOCAL   = 'A'
     AND DOMVALUE_L = G_MODULE.
  IF SY-SUBRC NE 0.
    "(1) 必須要是設定於範圍 ZMODULE 的模組代碼才可正常執行
    "(2) 執行時必須要以交易代碼 ZXXCONFIG 執行，而不可直接執行
    "錯誤訊息：報表 $ 僅能透過表單或交易代碼執行
    MESSAGE S144(68) WITH SY-REPID DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0.
  ELSE.
     SY-TITLE = |{ G_MODULE }自定義設定|.
  ENDIF.
ENDFORM.
