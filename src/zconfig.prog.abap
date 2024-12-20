************************************************************************
* Program Name      : ZCONFIG
* Descriptions      : 提供各模組自定義設定的快速路徑
* Package           : ZCONFIG
* Reference Tables  : ZCONF_DIR, ZCONF_ITEM
* How To Use        : 請詳見 Word 使用手冊
************************************************************************
REPORT  zcustom_config.
************************************************************************
* Class Definitions
************************************************************************
CLASS lcl_tree_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_tree_link_click FOR EVENT link_click
        OF cl_gui_column_tree
        IMPORTING node_key item_name.
ENDCLASS.
************************************************************************
* Types Definitions
************************************************************************
TYPES:BEGIN OF ty_item,
        dir_no    TYPE zconf_dir-dir_no,
        dir_text  TYPE zconf_dir-dir_text,
        item_no   TYPE zconf_item-item_no,
        item_text TYPE zconf_item-item_text,
        tabname   TYPE zconf_item-tabname,
        ztype     TYPE zconf_item-ztype,
        progname  TYPE zconf_item-progname,
        formname  TYPE zconf_item-formname,
      END OF ty_item.
************************************************************************
* Constants Definitions
************************************************************************
CONSTANTS:
  gc_sm30        TYPE serptype   VALUE 'SM30',
  gc_sm34        TYPE serptype   VALUE 'SM34',
  gc_se16        TYPE serptype   VALUE 'SE16',
  gc_se38        TYPE serptype   VALUE 'SE38',
  gc_cus_conf    TYPE string     VALUE 'CUS_CONF',
  gc_column_btn  TYPE tv_itmname VALUE 'BUTTON',
  gc_column_item TYPE tv_itmname VALUE 'ITEM'.
************************************************************************
* Data Definitions
************************************************************************
DATA: g_module         TYPE zmodule,
      gt_config        TYPE SORTED TABLE OF ty_item WITH UNIQUE KEY dir_no item_no,
      go_docking_tree  TYPE REF TO cl_gui_docking_container,
      go_tree          TYPE REF TO cl_gui_column_tree,
      go_event_tree    TYPE REF TO lcl_tree_event_receiver,
      gt_sel_condition TYPE TABLE OF vimsellist WITH HEADER LINE.
************************************************************************
* Class Implementation
************************************************************************
CLASS lcl_tree_event_receiver IMPLEMENTATION.
  METHOD handle_tree_link_click.
    PERFORM handle_tree_link_click USING node_key item_name.
  ENDMETHOD.
ENDCLASS.
************************************************************************
*  Selection Screen
************************************************************************
SELECTION-SCREEN PUSHBUTTON 1(20) refresh USER-COMMAND refresh.
************************************************************************
*  At Selection Screen Output
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM exclude_std_gui_status.
  PERFORM create_tree.
  PERFORM display_data.
************************************************************************
*  Initialization
************************************************************************
INITIALIZATION.
  PERFORM ini_data.
  PERFORM get_data.
************************************************************************
*  At Selection Screen
************************************************************************
AT SELECTION-SCREEN.
  IF sy-ucomm = 'REFRESH'.
    PERFORM get_data.
    PERFORM display_data.
  ENDIF.
*&---------------------------------------------------------------------*
*& Form Get_data
*&---------------------------------------------------------------------*
*& 透過自定義設定目錄(ZCONF_DIR)與設定項目表格(ZCONF_ITEM)取得資料
*&---------------------------------------------------------------------*
FORM get_data.
  SELECT
    FROM zconf_dir AS dir
    LEFT JOIN zconf_item AS item ON item~zmodule = dir~zmodule
                                AND item~dir_no  = dir~dir_no
    FIELDS dir~dir_no  , dir~dir_text, item~item_no , item~item_text,
           item~tabname, item~ztype  , item~progname, item~formname
  WHERE dir~zmodule = @g_module
  INTO TABLE @gt_config.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Create_tree
*&---------------------------------------------------------------------*
*& 建立樹狀結構，透過類別 CL_GUI_DOCKING_CONTAINER 建立
*&---------------------------------------------------------------------*
FORM create_tree.
  DATA: ls_header TYPE treev_hhdr,
        lt_events TYPE cntl_simple_events.
  CHECK go_docking_tree IS NOT BOUND.
  "劃立樹狀結構範圍
  CREATE OBJECT go_docking_tree
    EXPORTING
      ratio = 93
      side  = cl_gui_docking_container=>dock_at_bottom.
  "建立樹狀結構
  ls_header-heading = |{ g_module } 自定義設定|.
  ls_header-width = 50.
  CREATE OBJECT go_tree
    EXPORTING
      parent                = go_docking_tree
      node_selection_mode   = cl_gui_column_tree=>node_sel_mode_single
      item_selection        = abap_true
      hierarchy_column_name = gc_column_item
      hierarchy_header      = ls_header.
  "取得事件並加入事件
  go_tree->get_registered_events( IMPORTING events = lt_events ).
  APPEND VALUE #( eventid = cl_gui_column_tree=>eventid_link_click appl_event = 'X' ) TO lt_events.
  go_tree->set_registered_events( events = lt_events ).
  "建立事件處理類別
  CREATE OBJECT go_event_tree.
  SET HANDLER go_event_tree->handle_tree_link_click FOR go_tree.
  "在樹狀節點中加入按鈕
  go_tree->insert_hierarchy_column( name =  gc_column_btn ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Display_Data
*&---------------------------------------------------------------------*
*& 顯示資料
*&---------------------------------------------------------------------*
FORM display_data.
  DATA: lt_nodes    TYPE treev_ntab,
        lt_items    TYPE STANDARD TABLE OF streeitm WITH DEFAULT KEY,
        lv_node_key TYPE tv_nodekey.
  DEFINE macro_append_node.
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
  DEFINE macro_append_btn.
    APPEND VALUE #( node_key  = &1
                    item_name = gc_column_btn
                    class     = cl_gui_column_tree=>item_class_link
                    t_image   = icon_execute_object ) TO lt_items.
  END-OF-DEFINITION.
  DEFINE macro_append_item.
    APPEND VALUE #( node_key  = &1
                    item_name = gc_column_item
                    text      = &2
                    disabled  = abap_false ) TO lt_items.
  END-OF-DEFINITION.
  "刪除當前所有樹狀節點
  go_tree->delete_all_nodes( ).
  "新增程式「固定」項目
  macro_append_node gc_cus_conf '' icon_space ''.
  macro_append_btn  gc_cus_conf.
  macro_append_item gc_cus_conf '管理自定義設定'.
  "新增按「交易代碼」項目
  LOOP AT gt_config ASSIGNING FIELD-SYMBOL(<fs_item>).
    AT NEW dir_no.
      macro_append_node <fs_item>-dir_no '' '' 'X'.
      macro_append_item <fs_item>-dir_no <fs_item>-dir_text.
    ENDAT.
    IF <fs_item>-item_no IS NOT INITIAL OR <fs_item>-item_text IS NOT INITIAL.
      lv_node_key = |{ <fs_item>-dir_no }-{ <fs_item>-item_no }|.
      macro_append_node lv_node_key <fs_item>-dir_no icon_space ''.
      macro_append_btn  lv_node_key.
      macro_append_item lv_node_key <fs_item>-item_text.
    ENDIF.
  ENDLOOP.
  CALL METHOD go_tree->add_nodes_and_items
    EXPORTING
      node_table                = lt_nodes
      item_table                = lt_items
      item_table_structure_name = 'STREEITM'
    EXCEPTIONS
      OTHERS                    = 1.
  go_tree->expand_root_nodes( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Handle_Tree_Link_Click
*&---------------------------------------------------------------------*
*& 處理樹狀資料點擊事件
*&---------------------------------------------------------------------*
FORM handle_tree_link_click  USING  p_node_key TYPE lvc_nkey
                                  p_item_name TYPE tv_itmname.
  CASE p_node_key.
    WHEN gc_cus_conf.
      REFRESH gt_sel_condition.
      APPEND VALUE #( viewfield = 'ZMODULE' operator = 'EQ' value = g_module ) TO gt_sel_condition.
      PERFORM call_sm34 USING 'ZCONF'.
      PERFORM get_data.
      PERFORM display_data.
    WHEN OTHERS.
      CHECK strlen( p_node_key ) > 3.
      SPLIT p_node_key AT '-' INTO DATA(lv_dir_no) DATA(lv_item_no).
      READ TABLE gt_config WITH KEY dir_no = lv_dir_no item_no = lv_item_no ASSIGNING FIELD-SYMBOL(<fs_item>).
      IF <fs_item>-ztype = gc_sm30.
        PERFORM check_maintenance USING <fs_item>-tabname.
        PERFORM call_sm30         USING <fs_item>-tabname.
      ELSEIF <fs_item>-ztype = gc_sm34.
        PERFORM check_maintenance_cluster USING <fs_item>-tabname.
        PERFORM call_sm34         USING <fs_item>-tabname.
      ELSEIF <fs_item>-ztype = gc_se16.
        PERFORM check_tables      USING <fs_item>-tabname.
        PERFORM call_se16         USING <fs_item>-tabname.
      ELSEIF <fs_item>-ztype = gc_se38.
        PERFORM check_progname    USING <fs_item>-progname.
        PERFORM call_se38         USING <fs_item>-progname.
      ELSE.
        PERFORM (<fs_item>-formname) IN PROGRAM (<fs_item>-progname) IF FOUND.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Call_SM30
*&---------------------------------------------------------------------*
*& 呼叫 SM30 編輯模式（U：編輯｜S：顯示）
*&---------------------------------------------------------------------*
FORM call_sm30 USING p_tablename TYPE dd02v-tabname.
  DATA l_action TYPE c VALUE 'U'.
  IF gt_sel_condition[] IS INITIAL.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action    = l_action
        view_name = p_tablename.
  ELSE.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action      = l_action
        view_name   = p_tablename
      TABLES
        dba_sellist = gt_sel_condition[].
    REFRESH: gt_sel_condition.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Call_SM34
*&---------------------------------------------------------------------*
*& 呼叫 SM34 編輯模式（U：編輯｜S：顯示）
*&---------------------------------------------------------------------*
FORM call_sm34 USING p_clustername TYPE vcldir-vclname.
  DATA: l_action TYPE c VALUE 'U'.
  IF gt_sel_condition[] IS INITIAL.
    CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
      EXPORTING
        maintenance_action = 'U'
        viewcluster_name   = p_clustername.
  ELSE.
    CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
      EXPORTING
        maintenance_action = 'U'
        viewcluster_name   = p_clustername
      TABLES
        dba_sellist        = gt_sel_condition[].
    REFRESH: gt_sel_condition.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Call_SE16
*&---------------------------------------------------------------------*
*& 呼叫 SE16 檢視模式
*&---------------------------------------------------------------------*
FORM call_se16 USING p_tablename TYPE dd02v-tabname.

  CALL FUNCTION 'RS_TABLE_LIST_CREATE'
    EXPORTING
      table_name = p_tablename.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Call_SE38
*&---------------------------------------------------------------------*
*& 呼叫 SE38 執行程式
*&---------------------------------------------------------------------*
FORM call_se38 USING in_progname TYPE reposrc-progname.

  SUBMIT (in_progname) VIA SELECTION-SCREEN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCLUDE_STD_GUI_STATUS
*&---------------------------------------------------------------------*
*& 清除標準 GUI 狀態：SPOS（儲存）、ONLI（執行）
*&---------------------------------------------------------------------*
FORM exclude_std_gui_status.
  DATA:
  lt_exclude  TYPE TABLE OF sy-ucomm.
  lt_exclude = VALUE #( ( 'ONLI' ) ( 'SPOS' ) ).
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
      p_program = sy-cprog
    TABLES
      p_exclude = lt_exclude.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INI_DATA
*&---------------------------------------------------------------------*
*& 從交易代碼初始化資料
*&---------------------------------------------------------------------*
FORM ini_data.
  DATA module_name TYPE domvalue_l.
* 設定當前按鈕之中文說明
  refresh = '頁面刷新'(b01).
* 將交易代碼首位數去除，可能是「Z」或「Y」。並將 CONFIG 字樣去除
  "(1) 必須要是設定於範圍 ZMODULE 的模組代碼才可正常執行
  "(2) 執行時必須要以交易代碼 ZXXCONFIG 執行，而不可直接執行
  PERFORM get_module_name CHANGING module_name.
  SELECT SINGLE COUNT( * )
    FROM dd07l
   WHERE domname    = 'ZMODULE'
     AND as4local   = 'A'
     AND domvalue_l = module_name.
  IF sy-subrc NE 0.
    "錯誤訊息：報表 $ 僅能透過表單或交易代碼執行
    MESSAGE s144(68) WITH sy-repid DISPLAY LIKE 'E'. LEAVE TO SCREEN 0.
  ELSE.
    g_module = module_name.
    sy-title = |{ g_module } 自定義設定|.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_module_name
*&---------------------------------------------------------------------*
FORM get_module_name  CHANGING module_name TYPE domvalue_l.

  module_name = sy-tcode+1.
  REPLACE FIRST OCCURRENCE OF `CONFIG` IN module_name WITH ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_tables
*&---------------------------------------------------------------------*
*& 確認是否為啟用中表格？
*&---------------------------------------------------------------------*
FORM check_tables  USING in_tabname TYPE tabname.

  SELECT SINGLE COUNT( * )
    FROM dd02l
   WHERE tabname  = @in_tabname
     AND tabclass = 'TRANSP'
     AND actflag  = @space.
  IF sy-subrc NE 0.
    "28(151): 表格&1不存在
    MESSAGE e151(28) WITH in_tabname.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_maintenance
*&---------------------------------------------------------------------*
*& 確認是否為啟用中維護檢視
*&---------------------------------------------------------------------*
FORM check_maintenance  USING in_tabname TYPE tabname.

  DATA vim_name TYPE vim_name.

  vim_name = in_tabname.

  SELECT SINGLE COUNT( * )
    FROM tvdir
   WHERE tabname = @vim_name.
  IF sy-subrc NE 0.
    "164(SV): 表格 / 檢視 &1 不在字典中
    MESSAGE e164(sv) WITH in_tabname.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_maintenance_cluster
*&---------------------------------------------------------------------*
*& 確認是否為啟用中維護檢視叢集
*&---------------------------------------------------------------------*
FORM check_maintenance_cluster  USING in_tabname TYPE tabname.

  DATA vcl_name TYPE vcl_name.

  vcl_name = in_tabname.

  SELECT SINGLE COUNT( * )
    FROM vcldir
   WHERE vclname = @vcl_name.
  IF sy-subrc NE 0.
    "515(SV): 檢視叢集 & 不存在
    MESSAGE e515(sv) WITH in_tabname.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_progname
*&---------------------------------------------------------------------*
*& 確認是否存在程式？
*&---------------------------------------------------------------------*
FORM check_progname  USING in_progname type progname.

  SELECT SINGLE COUNT( * )
    FROM reposrc
   WHERE PROGNAME  = @in_progname
     AND R3STATE   = 'A'.
  IF sy-subrc NE 0.
    "017(DS): 程式 & 不存在
    MESSAGE e017(DS) WITH in_progname.
  ENDIF.

ENDFORM.
