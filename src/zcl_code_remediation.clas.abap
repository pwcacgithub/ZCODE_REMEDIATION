CLASS zcl_code_remediation DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_remediate,
        table         TYPE char5,
        value         TYPE char255,
        multiple      TYPE boolean,
        target        TYPE char255,
        row           TYPE int4,
        row_to        TYPE int4,
        forallentries TYPE boolean,
      END OF ty_remediate .
    TYPES:
      BEGIN OF TY_COMMENTs,
        row  TYPE token_row,
        code TYPE string,
      END OF ty_comments .
    TYPES:
      tt_comments TYPE STANDARD TABLE OF ty_comments .
    TYPES:
      BEGIN OF ty_obj_details,
        application         TYPE  rglif-appl,
        authorization_group TYPE  rglif-auth,
        development_class   TYPE  rglif-devclass,
        edit_lock           TYPE  rglif-edit_lock,
        log_db              TYPE  rglif-logdb,
        program_name        TYPE  rglif-include,
        program_type        TYPE  rglif-progtype,
        temporary           TYPE  rglif-temporary,
        title_string        TYPE  rglif-title,
        transport_number    TYPE  rglif-trkorr,
      END OF ty_obj_details .
    TYPES:
      BEGIN OF ty_message,
        line    TYPE i,
        typ     TYPE char1,
        message TYPE string,
      END OF ty_message .
    TYPES:
      BEGIN OF ty_source_map,
        level    TYPE stmnt_levl,
        name     TYPE level_name,
        src_code TYPE string_table,
      END OF ty_source_map .
    TYPES:
      tt_message TYPE STANDARD TABLE OF ty_message .
    TYPES:
      tt_source_map TYPE STANDARD TABLE OF ty_source_map .
    TYPES:
      BEGIN OF ty_object_details,
        src_object_name TYPE sobj_name,
        sub_object_name TYPE  sobj_name,
        object_type     TYPE  trobjtype,
      END OF ty_object_details .
    TYPES:
      tt_obj_det TYPE STANDARD TABLE OF ty_object_details .

    TYPES:
      BEGIN OF ty_obj_counter,
        object_name TYPE sobj_name,
        insert      TYPE i,
        comment     TYPE i,
        at_line     TYPE i,
        sequence    TYPE i,
        row         TYPE i,
        org_line    TYPE i,
      END OF ty_obj_counter .
    TYPES:
      tt_obj_counter TYPE STANDARD TABLE OF ty_obj_counter.

    DATA gt_obj_cntr TYPE SORTED TABLE OF ty_obj_counter  WITH NON-UNIQUE KEY object_name at_line.
    DATA gt_obj_cntr_temp TYPE SORTED TABLE OF ty_obj_counter  WITH NON-UNIQUE KEY object_name at_line.
    DATA gt_log_messages TYPE bal_t_msg .
    DATA gt_messages TYPE tt_message .
    DATA gt_org_src_cod TYPE tt_source_map .
    DATA gv_object_name TYPE sobj_name .
    DATA gv_is_fugr TYPE boolean .
    DATA gv_is_func TYPE boolean .
    DATA gv_fugr_name TYPE sobj_name .
    DATA gv_obj_type TYPE trobjtype .
    DATA gt_object_details TYPE tt_obj_det .
    DATA gv_apply_changes TYPE boolean .
    DATA gt_result TYPE ztt_code_remediation_result .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_code_remediation .
    METHODS execute
      IMPORTING
        !im_object        TYPE sobj_name OPTIONAL
        !im_objtyp        TYPE trobjtype OPTIONAL
        !im_transportno   TYPE trkorr OPTIONAL
        !im_package       TYPE devclass OPTIONAL
        !im_runseries     TYPE satc_ac_resulth-run_series_name OPTIONAL
        !im_cnfrm_remed   TYPE ztt_code_remediation_result OPTIONAL
        !im_apply_changes TYPE boolean OPTIONAL
      EXPORTING
        !ex_message       TYPE tt_message
        !ex_result        TYPE ztt_code_remediation_result .
    METHODS process_application_log
      IMPORTING
        !im_object  TYPE sobj_name
        !ex_message TYPE tt_message .
protected section.
private section.

  class-data GO_INSTANCE type ref to ZCL_CODE_REMEDIATION .
  class-data GV_COMMENT type STRING .
  data GT_REMEDIATION type ZTT_CODE_REMEDIATION_RESULT .
  data GV_SEQ type INT4 .
  class-data GV_TESTRUN type BOOLEAN .

  methods READ_REMEDIATION
    importing
      !IM_OBJECT type SOBJ_NAME optional
      !IM_OBJTYP type TROBJTYPE optional
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME optional
    exporting
      !EX_MESSAGE type TT_MESSAGE
      !EV_INSERT type FLAG .
  methods CODE_SCAN
    importing
      !IM_OBJECT type SOBJ_NAME
    exporting
      !EX_SCAN type ref to CL_CI_SCAN
      !ET_SOURCE_MAP type TT_SOURCE_MAP .
  methods PROG_SYNTAX_CHECK
    importing
      !IM_OBJECT type SOBJ_NAME
      !IM_SRCCODE type STRING_TABLE
    exporting
      !ET_WARNING type SYNT_ERRORS
      !EX_LINE type I
      !EX_WORD type STRING
      !EX_MSG type STRING
      !EX_DIR type TRDIR .
  methods SELECT_SINGLE_REMEDIATION
    importing
      !IM_OBJECT type SOBJ_NAME
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME
    exporting
      !EX_MESSAGE type TT_MESSAGE
      !EV_INSERT type FLAG .
  methods READ_WRITE_ON_DB_REMEDIATION
    importing
      !IM_OBJECT type SOBJ_NAME optional
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME optional
    exporting
      !EX_MESSAGE type TT_MESSAGE
      !EV_INSERT type FLAG .
  methods GET_OBJ_DETAIL
    importing
      value(IM_OBJECT) type SOBJ_NAME
      !IM_TRANSPORTNO type TRKORR optional
      !IM_PACKAGE type DEVCLASS optional
    exporting
      value(ES_OBJ_DETAILS) type TY_OBJ_DETAILS .
  methods FOR_ALL_ENTRIES_REMEDIATION
    importing
      !IM_OBJECT type SOBJ_NAME
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME
    exporting
      !EV_INSERT type FLAG
      !EX_MESSAGE type TT_MESSAGE .
  methods GENERATE_PROGRAM
    importing
      value(IS_OBJ_DETAILS) type TY_OBJ_DETAILS optional
    exporting
      value(EX_MESSAGE) type TT_MESSAGE .
  methods ADD_COMMENTS
    returning
      value(RE_COMMENT) type STRING .
  methods WRITE_TO_KONV
    importing
      !IM_OBJECT type SOBJ_NAME optional
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME optional
    exporting
      !EX_MESSAGE type TT_MESSAGE
      !EV_INSERT type FLAG .
  methods CONSTRUCTOR .
  methods REPLACE_VBUK_VBUP
    importing
      !IM_OBJECT type SOBJ_NAME
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME
    exporting
      !EV_INSERT type FLAG
      !EX_MESSAGE type TT_MESSAGE .
  methods CODE_WRAP
    importing
      !IM_OBJECT type SOBJ_NAME .
  methods LOOP_REMEDIATION
    importing
      !IM_OBJECT type SOBJ_NAME optional
      !IM_OBJTYP type TROBJTYPE optional
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME optional
    exporting
      !EX_MESSAGE type TT_MESSAGE
      !EV_INSERT type FLAG .
  methods REPLACE_MATNR_DE
    importing
      !IM_OBJECT type SOBJ_NAME
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME
    exporting
      !EX_MESSAGE type TT_MESSAGE
      !EV_INSERT type FLAG .
  methods DELETE_DUPLICATE
    importing
      !IM_OBJECT type SOBJ_NAME optional
      !IM_OBJTYP type TROBJTYPE optional
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME optional
    exporting
      !EX_MESSAGE type TT_MESSAGE
      !EV_INSERT type FLAG .
  methods LOOP_AT_NEW
    importing
      !IM_OBJECT type SOBJ_NAME
      !IM_OBJTYP type TROBJTYPE
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME
    exporting
      !EX_MESSAGE type TT_MESSAGE
      !EV_INSERT type FLAG .
  methods PROCESS_PRETTY_PRINTER
    importing
      !IM_OBJECT type SOBJ_NAME
      !IM_RUNSERIES type SATC_AC_RESULTH-RUN_SERIES_NAME
    exporting
      !EV_INSERT type FLAG
      !EX_MESSAGE type TT_MESSAGE .
  methods METH_FETCH_REMEDIATE_OBJ
    importing
      !IM_OBJECT type SOBJ_NAME
    exporting
      !EX_REM_OBJ type SOBJ_NAME
      !EX_ERROR type BOOLEAN
      !EX_SET_PRETY_PRINT type BOOLEAN
    changing
      !CH_OBJTYP type TROBJTYPE .
  methods METH_PREP_LOG_MSG
    importing
      !IM_REMEDIATION type CHAR50
      !IM_REM_OBJ_NAME type SOBJ_NAME
      !IM_OLD_CODE_START_LINE type CHAR10
      !IM_OLD_CODE_END_LINE type CHAR10
      !IM_NEW_CODE_START_LINE type CHAR10
      !IM_NEW_CODE_END_LINE type CHAR10
      !IM_T_OLD_SRC_CODE type STRING_TABLE
      !IM_T_NEW_SRC_CODE type STRING_TABLE .
  methods METH_VALIDATE_SRC_CODE
    importing
      !IM_OBJECT type SOBJ_NAME
      !IM_T_SRC_CODE type STRING_TABLE
    exporting
      !EX_ERROR type BOOLEAN .
  methods METH_GET_DELTA_CHANGES
    importing
      !IM_OBJECT type SOBJ_NAME .
  methods METH_CAPTURE_OBJ
    importing
      !IM_OBJECT type SOBJ_NAME
      !IM_TRANSPORTNO type TRKORR optional .
  methods GET_COUNTER
    importing
      !IM_OBJ_NAME type SOBJ_NAME
      !IM_LINE_NO type INT4
    exporting
      !EX_COUNTER type INT4 .
  methods SET_COUNTER
    importing
      !IM_OBJ_NAME type SOBJ_NAME
      !IM_LINE_NO type INT4
      !IM_LINES_ADD type INT4
      !IM_ROW type INT4 optional
      !IM_ORG_LINE type INT4 optional
      !IM_COMMENT_LINE type INT4 optional .
  methods ADJUST_COUNTER .
ENDCLASS.



CLASS ZCL_CODE_REMEDIATION IMPLEMENTATION.


  METHOD write_to_konv.

** Structure
    TYPES: BEGIN OF ty_operation,
             operation TYPE char10,
           END OF ty_operation.
** Constants
    CONSTANTS: lc_select        TYPE string VALUE 'SELECT',
               lc_delete        TYPE string VALUE 'DELETE',
               lc_insert        TYPE string VALUE 'INSERT',
               lc_update        TYPE string VALUE 'UPDATE',
               lc_modify        TYPE string VALUE 'MODIFY',
               lc_prcd_elements TYPE string VALUE 'PRCD_ELEMENTS',
               lc_konv          TYPE string VALUE 'KONV'.
** Data
    DATA: lt_operation   TYPE STANDARD TABLE OF ty_operation,
          itab           TYPE STANDARD TABLE OF string,
          lr_scan        TYPE REF TO cl_ci_scan,
          lv_line        TYPE i,
          ls_word        TYPE string,
          ls_dir         TYPE trdir,
          ls_msg         TYPE string,
          lt_warnings    TYPE synt_errors,
          lv_fld         TYPE string,
          lt_source_map  TYPE tt_source_map,
          ls_obj_details TYPE ty_obj_details,
          lin            TYPE i,
          wrd            TYPE string,
          dir            TYPE trdir,
          msg            TYPE string,
          uc             TYPE trdir-uccheck,
          lv_count       TYPE i,
          lv_string      TYPE string.

** Clearing the exporting parameters.
    CLEAR: ev_insert,ex_message.

** Code Scan
    CALL METHOD me->code_scan
      EXPORTING
        im_object     = im_object
      IMPORTING
        ex_scan       = lr_scan
        et_source_map = lt_source_map.

** Filling the operation table.
    APPEND INITIAL LINE TO lt_operation ASSIGNING FIELD-SYMBOL(<lfs_operation>).
    <lfs_operation>-operation = lc_insert.
    APPEND INITIAL LINE TO lt_operation ASSIGNING <lfs_operation>.
    <lfs_operation>-operation = lc_delete.
    APPEND INITIAL LINE TO lt_operation ASSIGNING <lfs_operation>.
    <lfs_operation>-operation = lc_modify.
    APPEND INITIAL LINE TO lt_operation ASSIGNING <lfs_operation>.
    <lfs_operation>-operation = lc_update.
    APPEND INITIAL LINE TO lt_operation ASSIGNING <lfs_operation>.
    <lfs_operation>-operation = lc_select.

** Read the program code
    LOOP AT lr_scan->levels ASSIGNING FIELD-SYMBOL(<ls_levels>).
      APPEND INITIAL LINE TO lt_source_map ASSIGNING FIELD-SYMBOL(<ls_source_map>).
      ASSIGN <ls_source_map>-src_code TO FIELD-SYMBOL(<lt_src_code>).
      lv_count = lv_count + 1.
      <ls_source_map>-level = lv_count.
      <ls_source_map>-name = <ls_levels>-name.
      READ REPORT <ls_levels>-name INTO <lt_src_code>.
    ENDLOOP.

** FInd the insert/update/modify etc and check the table name
    LOOP AT lt_operation ASSIGNING <lfs_operation>.
      LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_scan>) WHERE str = <lfs_operation>-operation.
        DATA(lv_indx) = sy-tabix.
*        LOOP AT lt_tabname ASSIGNING FIELD-SYMBOL(<lfs_tabname>).
        LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_scan1>) FROM lv_indx  WHERE str = lc_konv.
          DATA(lv_index) = sy-tabix.
          DATA(lv_row) = <ls_scan1>-row.
          LOOP AT lr_scan->statements ASSIGNING FIELD-SYMBOL(<lfs_keys>)
                                                   WHERE from <= lv_index AND
                                                         to   >= lv_index.
          ENDLOOP.
          IF <lfs_keys> IS ASSIGNED.
            READ TABLE lt_source_map ASSIGNING <ls_source_map> WITH KEY level = <lfs_keys>-level.
            IF sy-subrc IS INITIAL.
              READ TABLE <ls_source_map>-src_code ASSIGNING FIELD-SYMBOL(<ls_src_code>) INDEX lv_row.
              IF sy-subrc IS INITIAL.
                lv_string = <ls_src_code>.
                TRANSLATE lv_string TO UPPER CASE.
                FIND lc_konv IN  lv_string.
                IF sy-subrc IS INITIAL.
** Only for reading the data
                  CASE <lfs_operation>-operation.
                    WHEN lc_select.
                      DATA(lv_old_string) = lv_string.

                      REPLACE lc_konv IN lv_string  WITH lc_prcd_elements.
                    WHEN lc_delete.
                    WHEN lc_insert.
                    WHEN lc_update.
                    WHEN lc_modify.
                    WHEN OTHERS.
                  ENDCASE.
*                  IF <lfs_operation>-operation = lc_select.
*                    REPLACE lc_konv IN lv_string  WITH lc_prcd_elements.
*                  ELSE.
*** Only for writing to DB
*                    REPLACE lc_konv IN lv_string  WITH 'PRCD_ELEMENTS'.
*                  ENDIF.
*                    CONCATENATE lv_string lc_upd_comment INTO <ls_src_code> RESPECTING BLANKS.
                  DATA(lv_insert) = abap_true.
                  <ls_src_code> = lv_string.
*                  DATA(lv_length) = strlen( lv_string ).
*                  IF lv_length > 72.
*
*                  ENDIF.

** Comment the old code
                  CONCATENATE '"' lv_old_string INTO lv_old_string.
                  INSERT lv_old_string INTO <ls_source_map>-src_code INDEX lv_row.
** Update the line with new code.
                  INSERT REPORT <ls_source_map>-name FROM <ls_source_map>-src_code.
                ENDIF.
** Syntax Check for the updated program
                CALL METHOD me->prog_syntax_check
                  EXPORTING
                    im_object  = im_object
                    im_srccode = <ls_source_map>-src_code
                  IMPORTING
                    et_warning = lt_warnings
                    ex_line    = lv_line
                    ex_word    = ls_word
                    ex_msg     = ls_msg
                    ex_dir     = ls_dir.
*                IF ls_msg IS NOT INITIAL.
*                  APPEND INITIAL LINE TO ex_message ASSIGNING FIELD-SYMBOL(<lfs_message>).
*                  <lfs_message>-line = lv_line.
*                  <lfs_message>-message = ls_msg.
*                  IF <lfs_message>-line IS NOT INITIAL.
*                    CLEAR: lv_insert.
*                  ENDIF.
*                ENDIF.
                IF lt_warnings IS NOT INITIAL.
                  LOOP AT lt_warnings ASSIGNING FIELD-SYMBOL(<fs_warning>).
                    APPEND INITIAL LINE TO ex_message ASSIGNING FIELD-SYMBOL(<lfs_message>).
                    <lfs_message>-line = <fs_warning>-line.
                    <lfs_message>-message = <fs_warning>-message.
*        IF lv_line EQ 0 AND ls_msg IS NOT INITIAL.
*          <lfs_message>-typ = lc_info.
*          COMMIT WORK AND WAIT.
*      IF lv_line NE 0 AND ls_msg IS NOT INITIAL.
                    <lfs_message>-typ = 'E'.
                  ENDLOOP.
                  CLEAR : ev_insert.
                ELSE.
                  APPEND INITIAL LINE TO ex_message ASSIGNING <lfs_message>.
                  <lfs_message>-typ = 'S'.
                  <lfs_message>-message = TEXT-000.
                  COMMIT WORK AND WAIT.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
          CLEAR: lv_row,lv_index,lv_string.
        ENDLOOP.
*        ENDLOOP.
        CLEAR: lv_indx  .
      ENDLOOP.
    ENDLOOP.


** Syntax Check for the updated program : Main program
*    READ TABLE lt_source_map ASSIGNING <ls_source_map> WITH KEY level = 1.
*    IF sy-subrc EQ 0.
*      CALL METHOD me->prog_syntax_check
*        EXPORTING
*          im_object  = im_object
*          im_srccode = <ls_source_map>-src_code
*        IMPORTING
*          et_warning = lt_warnings
*          ex_line    = lv_line
*          ex_word    = ls_word
*          ex_msg     = ls_msg
*          ex_dir     = ls_dir.
*      IF ls_msg IS NOT INITIAL.
*        APPEND INITIAL LINE TO ex_message ASSIGNING FIELD-SYMBOL(<lfs_message>).
*        <lfs_message>-line = lv_line.
*        <lfs_message>-message = ls_msg.
*      ENDIF.
*    ENDIF.

    ev_insert = lv_insert.

    CLEAR: lv_insert,ls_dir,ls_msg,lv_line,lt_source_map,
    lt_warnings,lv_row,lv_index,lv_string,ls_word.

  ENDMETHOD.


  METHOD code_scan.

    DATA : gr_source TYPE REF TO cl_ci_source_include,
           lv_count  TYPE i.

    CLEAR: et_source_map.
    FREE  ex_scan.

      gr_source = cl_ci_source_include=>create( p_name = im_object ).

      CREATE OBJECT ex_scan
        EXPORTING
          p_include = gr_source.

    "Copy the whole program code into internal table with READ REPORT
    LOOP AT ex_scan->levels ASSIGNING FIELD-SYMBOL(<ls_levels>).
      APPEND INITIAL LINE TO et_source_map ASSIGNING FIELD-SYMBOL(<ls_source_map>).
      ASSIGN <ls_source_map>-src_code TO FIELD-SYMBOL(<lt_src_code>).
      lv_count = lv_count + 1.
      <ls_source_map>-level = lv_count.
      <ls_source_map>-name = <ls_levels>-name.
      READ REPORT <ls_levels>-name INTO <lt_src_code>.

    ENDLOOP.


  ENDMETHOD.


  METHOD loop_remediation.
    CONSTANTS : lc_loop        TYPE string VALUE 'LOOP',
                lc_where       TYPE string VALUE 'WHERE',
                lc_binary_srch TYPE string VALUE '*BINARY SEARCH.',
                lc_period      TYPE string VALUE '.',
                lc_new         TYPE string VALUE 'NEW',
                lc_index       TYPE string VALUE '*INDEX*.',
                lc_sort        TYPE string VALUE 'SORT',
                lc_by          TYPE string VALUE 'BY',
                lc_eq          TYPE string VALUE '=',
                lc_info        TYPE char1 VALUE 'I',
                lc_error       TYPE char1 VALUE 'E',
                lc_success     TYPE char1 VALUE 'S'.

    DATA : lr_scan        TYPE REF TO cl_ci_scan,
           lv_line        TYPE i,
           lv_counter     TYPE i,
           ls_word        TYPE string,
           lv_pgm_name    TYPE string,
           ls_dir         TYPE trdir,
           ls_msg         TYPE string,
           lt_warnings    TYPE synt_errors,
           lv_fld         TYPE string,
           lt_source_map  TYPE tt_source_map,
           lt_source_code TYPE string_table.

    "Code Scan
    CALL METHOD me->code_scan
      EXPORTING
        im_object     = im_object
      IMPORTING
        ex_scan       = lr_scan
        et_source_map = lt_source_map.


    "Fetch statements with LOOP query
    LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_scan>) WHERE str = lc_loop.
      DATA(lv_index) = sy-tabix.
      DATA(lv_row_read) = <ls_scan>-row.

      READ TABLE lr_scan->tokens INTO DATA(ls_token) INDEX lv_index + 1.
      IF sy-subrc = 0 AND ls_token-str = 'AT'.
********* Read int_tab name from LOOP statement.
        READ TABLE lr_scan->tokens INTO DATA(ls_tok_scan) INDEX lv_index + 2.
        IF sy-subrc EQ 0.
          DATA(lv_sort1) = | { lc_sort } { ls_tok_scan-str } { lc_by } |.
        ENDIF.
        READ TABLE lr_scan->statements ASSIGNING FIELD-SYMBOL(<lfs_keys>)
                                                WITH KEY from = lv_index.
        IF sy-subrc EQ 0.
          "Get the fields from LOOP statement to use in SORT
          LOOP AT lr_scan->tokens INTO ls_tok_scan FROM lv_index WHERE str = lc_where.
            DATA(lv_key_ind) = sy-tabix + 1.
            DATA(lv_sort) = lv_sort1.

            WHILE lv_key_ind <= <lfs_keys>-to.
              READ TABLE lr_scan->tokens INTO DATA(ls_tokens) INDEX lv_key_ind.
              IF sy-subrc EQ 0.
                DATA(lv_idx) = lv_key_ind.
                lv_key_ind = lv_key_ind + 4.
                READ TABLE lr_scan->tokens INTO DATA(ls_tokens_temp) INDEX lv_idx + 1.
                IF sy-subrc EQ 0 AND ls_tokens_temp-str EQ lc_eq.
                  lv_fld = |{ lv_fld } { ls_tokens-str }|.
                ENDIF.
              ENDIF.
            ENDWHILE.
            EXIT.
          ENDLOOP.

        ENDIF.
        IF lv_fld IS NOT INITIAL.
          CONDENSE lv_fld.
          "Generate SORT statement
          lv_sort = |{ lv_sort }{ lv_fld } .|.
          CONDENSE lv_sort.
          "Get end line of one statement
          LOOP AT lr_scan->statements ASSIGNING FIELD-SYMBOL(<ls_statements>)
                                                WHERE from <= lv_index AND
                                                      to   >= lv_index.
            DATA(lv_level) = <ls_statements>-level.
            DATA(lv_row) = <ls_statements>-trow.
            EXIT.
          ENDLOOP.

          "Place binary search in read statement
          READ TABLE lt_source_map ASSIGNING FIELD-SYMBOL(<ls_source_map>) WITH KEY level = lv_level.
          IF sy-subrc = 0 .
            IF lv_pgm_name <> <ls_source_map>-name.
              DATA(lt_source) = <ls_source_map>-src_code.
              CLEAR lv_counter.
            ENDIF.

            lv_pgm_name = <ls_source_map>-name.

            READ TABLE <ls_source_map>-src_code ASSIGNING FIELD-SYMBOL(<ls_src_code>) INDEX lv_row.
            IF sy-subrc = 0.
              IF <ls_src_code> CP lc_index.
              ELSEIF  <ls_src_code> CP lc_binary_srch.
*Case where Binary Search exists
*Put Sort statement before read with binary search.

*Check whether Sorting is Done Already
                DATA(lv_temp_cntr) = lv_row_read.
                DATA: lv_temp_code TYPE string.
                DO 3 TIMES.
                  READ TABLE <ls_source_map>-src_code
                          ASSIGNING <ls_src_code> INDEX lv_temp_cntr.

                  lv_temp_code = |{ <ls_src_code> } { lv_temp_code }|.
                  lv_temp_cntr = lv_temp_cntr - 1.
                ENDDO.

                CONDENSE lv_temp_code.
                TRANSLATE lv_temp_code TO UPPER CASE.
                TRANSLATE lv_sort TO UPPER CASE.

                IF lv_temp_code CS lv_sort .
                ELSE.
                  INSERT |{ lv_sort } { gv_comment }|
                         INTO lt_source INDEX lv_counter + lv_row_read.
                  lv_counter = lv_counter + 1.
                ENDIF.
                CLEAR: lv_temp_cntr, lv_temp_code.
              ELSE.
*Case where Binary Search does not exist
                DATA(len) = strlen( <ls_src_code> ) - 1 ..
                DATA(ls_temp) = |{ <ls_src_code>(len) } { lc_period }|.
                MODIFY lt_source FROM ls_temp INDEX lv_row + lv_counter.
                CLEAR ls_temp.
*Check whether Sorting is Done Already
                lv_temp_cntr = lv_row_read.
                DO 3 TIMES.
                  READ TABLE <ls_source_map>-src_code
                          ASSIGNING <ls_src_code> INDEX lv_temp_cntr.

                  lv_temp_code = |{ <ls_src_code> } { lv_temp_code }|.
                  lv_temp_cntr = lv_temp_cntr - 1.
                ENDDO.

                CONDENSE lv_temp_code.
                TRANSLATE lv_temp_code TO UPPER CASE.
                TRANSLATE lv_sort TO UPPER CASE.

                IF lv_temp_code CS lv_sort .
                ELSE.
                  INSERT |{ lv_sort } { gv_comment }|
                         INTO lt_source INDEX lv_counter + lv_row_read.
                  lv_counter = lv_counter + 1.
                ENDIF.
                CLEAR: lv_temp_cntr, lv_temp_code.

                "Update program
                INSERT REPORT <ls_source_map>-name FROM
                lt_source.
                ev_insert = abap_true.
              ENDIF.
            ENDIF.
          ENDIF.
          CLEAR : lv_sort,
                  lv_fld,
                  lv_key_ind,
                  ls_tok_scan,
                  ls_tokens_temp,
                  ls_msg,
                  lv_line.
        ENDIF.
      ENDIF.
    ENDLOOP.

*Syntax Check for the updated program : Main program
    READ TABLE lt_source_map ASSIGNING <ls_source_map>
                                    WITH KEY level = 1.
    IF sy-subrc EQ 0.
      CALL METHOD me->prog_syntax_check
        EXPORTING
          im_object  = im_object
          im_srccode = <ls_source_map>-src_code
        IMPORTING
          et_warning = lt_warnings
          ex_line    = lv_line
          ex_word    = ls_word
          ex_msg     = ls_msg
          ex_dir     = ls_dir.
*      IF lv_line NE 0 AND ls_msg IS NOT INITIAL.
      IF lt_warnings IS NOT INITIAL.
        LOOP AT lt_warnings ASSIGNING FIELD-SYMBOL(<fs_warning>).
          APPEND INITIAL LINE TO ex_message ASSIGNING FIELD-SYMBOL(<lfs_message>).
          <lfs_message>-line = <fs_warning>-line.
          <lfs_message>-message = <fs_warning>-message.
          <lfs_message>-typ = 'E'.
        ENDLOOP.
        CLEAR : ev_insert.
      ELSE.
        APPEND INITIAL LINE TO ex_message ASSIGNING <lfs_message>.
        <lfs_message>-typ = lc_success.
        <lfs_message>-message = TEXT-000.
        COMMIT WORK AND WAIT.
      ENDIF.
      IF lt_warnings IS NOT INITIAL.
      ENDIF.
    ENDIF.

    CLEAR : lv_sort,
        lv_fld,
        lv_key_ind,
        ls_tok_scan,
        ls_tokens_temp,
        ls_msg,
        lv_line.
  ENDMETHOD.


  METHOD replace_vbuk_vbup.

    "Declaring global variables
    DATA: gr_scan       TYPE REF TO cl_ci_scan,
          gr_source     TYPE REF TO cl_ci_source_include,
          gr_source_map TYPE tt_source_map,
          gt_comments   TYPE tt_comments.
    TYPES lt_vbuk_key_r TYPE RANGE OF vbak-vbeln.

    "Declare internal table and work area
    DATA: gt_token TYPE stokesx_tab.
    DATA: gt_token1 TYPE stokesx_tab.
    DATA: gs_token TYPE stokesx.

    DATA: lt_src_code TYPE TABLE OF string.
    DATA: lt_src TYPE TABLE OF string.

    "Decalring local variable
    DATA:lc_x TYPE char1 .
    DATA: lv_lines      TYPE i,
          "Variable to capture error message
          lv_line       TYPE i,
          ls_word       TYPE string,
          ls_dir        TYPE trdir,
          ls_msg        TYPE string,
          lt_warnings   TYPE synt_errors,
          lv_flag       TYPE c,
          lv_pgm_name   TYPE string,
          lv_old_code   TYPE string,
          lv_first_line TYPE sy-tabix.

    CONSTANTS:lc_success TYPE char1 VALUE 'S'.
*--Begin of "SJALLIPALL00
    "Local type declarations
    TYPES: BEGIN OF lty_remediate,
             level TYPE int4.
             INCLUDE TYPE zcl_code_remediation=>ty_remediate.
    TYPES: END OF lty_remediate.
*--End of "SJALLIPALL00
**********************************Insert New Code
    DATA: lv_string          TYPE string,
          lv_newcode         TYPE string,
          lv_forallentries   TYPE string,
          lv_field           TYPE string,
          ls_remediate       TYPE lty_remediate, "me->ty_remediate,                  "SJALLIPALL00
          lt_remediate       TYPE STANDARD TABLE OF lty_remediate, "."me->ty_remediate."SJALLIPALL00
          lt_new_src_code    TYPE string_table,
          lt_old_src_code    TYPE string_table,
          lt_remediation_obj TYPE SORTED TABLE OF zstr_code_remediation
                                  WITH NON-UNIQUE KEY remediation_type.
    DATA: lt_lines TYPE STANDARD TABLE OF i.
    "Code Scan

    CALL METHOD me->code_scan
      EXPORTING
        im_object     = im_object "<lfs_smap>-name
      IMPORTING
        ex_scan       = gr_scan
        et_source_map = gr_source_map.
    gv_seq += 1.
    LOOP AT gr_scan->statements INTO DATA(ls_stat).

      "READ TABLE gr_scan->levels ASSIGNING FIELD-SYMBOL(<ls_levels>) INDEX ls_stat-level.
      READ TABLE gr_source_map INTO DATA(lwa_src_map) WITH KEY level = ls_stat-level.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      IF gv_apply_changes EQ abap_true.
        CLEAR lt_lines.
        lt_lines = VALUE #( FOR lwa_remed IN gt_remediation WHERE  (
        obj_name = lwa_src_map-name AND
        remediation_type = 'REPLACE_VBUK_VBUP'    )
           ( lwa_remed-line_no )  ).
      ENDIF.
      "Getting tocken for the current statements.
      LOOP AT gr_scan->tokens INTO DATA(ls_token)
        FROM  ls_stat-from
        TO  ls_stat-to.
        IF sy-tabix EQ ls_stat-from.
          DATA(lv_src_line_from) =  ls_token-row.
        ENDIF.
        IF sy-tabix EQ ls_stat-to.
          DATA(lv_src_line_to) =  ls_token-row.
        ENDIF.
        APPEND ls_token TO gt_token.
        CONCATENATE lv_string ls_token-str INTO lv_string SEPARATED BY space.
        TRANSLATE lv_string TO UPPER CASE.
      ENDLOOP.
**--Get counter
      IF gv_apply_changes EQ abap_true.
        CALL METHOD me->get_counter
          EXPORTING
            im_obj_name = lwa_src_map-name
            im_line_no  = lv_src_line_from
          IMPORTING
            ex_counter  = DATA(lv_g_cntr).

        READ TABLE lt_lines TRANSPORTING NO FIELDS WITH
          KEY  table_line = lv_src_line_from - lv_g_cntr."<lfs_glb_cntr>-counter.
        IF sy-subrc NE 0.
          CLEAR: ls_remediate,
       lv_string,
       gt_token.
          CONTINUE.
        ENDIF.
      ENDIF.

      "Table
      IF lv_string CP '*+#FROM VBUK+*'.
        ls_remediate-table = 'VBUK'.
      ELSEIF lv_string CP '*+#FROM VBUP+*'.
        ls_remediate-table = 'VBUP'.
      ENDIF.

      IF ls_remediate-table IS NOT INITIAL.
        IF lv_string CP '*+#VBELN IN+*' OR
          lv_string CP '*+#"+#SELECT+*' OR
          lv_string CP '*"SELECT*' OR
          lv_string CP '*+#JOIN+*+#ON+*' OR
         lv_string CP '*+#WHERE+*+#AND+*' OR
         lv_string CP '*+#WHERE+*+#OR+' OR
         lv_string CP '*+#@DATA+*' .
          .
          "Do nothing incase range is specified or
          "Joins are used
          "Additional fields are passed to where clause in addition to VBELN
        ELSE.
          IF lv_string CP '*+#FOR ALL ENTRIES+*'.
            ls_remediate-forallentries = abap_true.
          ELSE.
            ls_remediate-forallentries = abap_false.
          ENDIF.

          "VBELN Value
          READ TABLE gt_token INTO gs_token WITH KEY str = 'WHERE'.
          IF sy-subrc = 0.
            DATA(lv_tabix) = sy-tabix.
            lv_tabix = lv_tabix + 1.
            READ TABLE gt_token INTO gs_token INDEX lv_tabix." WITH KEY str = 'VBELN'.
            IF sy-subrc = 0 AND gs_token-str = 'VBELN'.
              DATA(lv_tabix1) = sy-tabix.
              lv_tabix1 = lv_tabix1 + 2.
              READ TABLE gt_token INTO gs_token INDEX lv_tabix1.
              IF sy-subrc = 0.
                ls_remediate-value = gs_token-str.
              ENDIF.
            ENDIF.
          ENDIF.

          "Targettype and Target structure
          IF ( lv_string CP '*+#SELECT SINGLE+*' OR
               lv_string CP '*+#UPTO 1 ROWS+*' OR
               lv_string CP '*+#UP TO 1 ROWS+*' ) .

            ls_remediate-multiple = abap_false.

            READ TABLE gt_token INTO gs_token WITH KEY str = 'INTO'.
            IF sy-subrc = 0.
              lv_tabix = sy-tabix.
              lv_tabix = lv_tabix + 1.
              READ TABLE gt_token INTO gs_token INDEX lv_tabix.
              IF sy-subrc = 0.
                DATA(lv_targetstructure) = gs_token-str.
                IF lv_targetstructure+0(5) = '@DATA'.
                  DATA(lv_length) = strlen( lv_targetstructure ).
                  lv_length = lv_length - 7.
                  ls_remediate-target = lv_targetstructure+6(lv_length).
                ELSE.
                  ls_remediate-target = lv_targetstructure.
                ENDIF.
              ENDIF.
            ENDIF.

          ELSEIF lv_string CP '*+#INTO TABLE+*'.
            ls_remediate-multiple = abap_true.

            READ TABLE gt_token INTO gs_token WITH KEY str = 'TABLE'.
            IF sy-subrc = 0.
              lv_tabix = sy-tabix.
              lv_tabix = lv_tabix + 1.
              READ TABLE gt_token INTO gs_token INDEX lv_tabix.
              IF sy-subrc = 0.
                lv_targetstructure = gs_token-str.
                IF lv_targetstructure+0(5) = '@DATA'.
                  lv_length = strlen( lv_targetstructure ).
                  lv_length = lv_length - 7.
                  ls_remediate-target = lv_targetstructure+6(lv_length).
                ELSE.
                  ls_remediate-target = lv_targetstructure.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          "Row
          READ TABLE gr_scan->tokens INTO gs_token INDEX ls_stat-from.
          IF sy-subrc = 0.
            ls_remediate-row = gs_token-row.
          ENDIF.

          READ TABLE gr_scan->tokens INTO gs_token INDEX ls_stat-to.
          IF sy-subrc = 0.
            ls_remediate-row_to = gs_token-row.
            DATA(final_row_next) = ls_stat-to + 1.
            READ TABLE gr_scan->tokens INTO gs_token INDEX final_row_next.
*            IF gs_token-str  CP '+ENDSELECT+' OR gs_token-str CP '+#ENDSELECT+*'.
            IF gs_token-str CS 'ENDSELECT'.
              ls_remediate-row_to = gs_token-row.
            ENDIF.
          ENDIF.

          ls_remediate-level = ls_stat-level.                           "SJALLIPALL00

          IF ls_remediate-table IS NOT INITIAL AND
             ls_remediate-value IS NOT INITIAL AND
             ls_remediate-target IS NOT INITIAL AND
             ls_remediate-row IS NOT INITIAL.
            APPEND ls_remediate TO lt_remediate.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR: ls_remediate,
             lv_string,
             gt_token.
    ENDLOOP.

    DATA lv_insert TYPE i .
    DATA lv_insert_new TYPE i .
    "   READ REPORT im_object INTO lt_src_code.                            "SJALLIPALL00
    IF lt_remediate IS NOT INITIAL.
**--Begin of      "SJALLIPALL00
      "LOOP AT lt_remediate ASSIGNING FIELD-SYMBOL(<ls_remediate>).     "SJALLIPALL00
      LOOP AT lt_remediate ASSIGNING FIELD-SYMBOL(<lfs_p_rem>)
            GROUP BY ( level =  <lfs_p_rem>-level )
            ASSIGNING FIELD-SYMBOL(<lfs_grp_rem>).
        READ TABLE gr_source_map WITH KEY level = <lfs_grp_rem>-level
          ASSIGNING FIELD-SYMBOL(<lfs_source>).
        IF lv_pgm_name <> <lfs_source>-name.
          lt_src_code = <lfs_source>-src_code.
          lv_pgm_name = <lfs_source>-name.
          CLEAR lv_insert.
        ENDIF.
        DATA(lt_temp_src) = lt_src_code.
        LOOP AT GROUP <lfs_grp_rem> ASSIGNING FIELD-SYMBOL(<ls_remediate>).
**--End of      "SJALLIPALL00
          READ TABLE lt_src_code ASSIGNING FIELD-SYMBOL(<ls_code_comment>) INDEX ( <ls_remediate>-row + lv_insert ).

*------------------------------------------------------------Below logic  is for commenting the existing logic---SA_1---------------------------------------------------------------------
          IF <ls_code_comment> CP '*+#"+#SELECT+*'.
            CONTINUE.
          ELSEIF <ls_code_comment> CP 'SELECT+*' OR <ls_code_comment> CP '*+#SELECT+*'.
            IF <ls_code_comment> CS '.'.
              CONCATENATE '"' <ls_code_comment> INTO <ls_code_comment>.
              lv_insert_new = lv_insert_new + 1.
              EXIT.
            ELSE.
              CONCATENATE '"' <ls_code_comment> INTO <ls_code_comment>.
              LOOP AT lt_src_code ASSIGNING <ls_code_comment> FROM <ls_remediate>-row + 1 + lv_insert.
                IF <ls_code_comment> CS '.'.
                  CONCATENATE '"' <ls_code_comment> INTO <ls_code_comment>.
                  lv_insert_new = lv_insert_new + 1.
                  EXIT.
                ELSE.
                  CONCATENATE '"' <ls_code_comment> INTO <ls_code_comment>.
                  lv_insert_new = lv_insert_new + 1.
                ENDIF.
              ENDLOOP.

              READ TABLE lt_src_code ASSIGNING FIELD-SYMBOL(<ls_code_comment_end>)
                   INDEX ( <ls_remediate>-row + lv_insert_new + lv_insert + 1 ).
              IF <ls_code_comment_end> CP 'ENDSELECT.+*' OR <ls_code_comment_end> CP '+#ENDSELECT.+*' .
                CONCATENATE '"' <ls_code_comment_end> INTO <ls_code_comment_end>.
                lv_insert_new = lv_insert_new + 1.
              ENDIF.
              DATA(lv_final_com) = <ls_remediate>-row + lv_insert_new.

            ENDIF.
          ENDIF.
          lv_insert = lv_insert + 1.
          INSERT gv_comment INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).
*----------------------------------------------------------------------SA_1---------------------------------------------------------
          DATA : lv_cur_row_s TYPE i.
          DATA : lv_cur_row_e TYPE i.
          DATA : lv_start_index TYPE i.
          DATA : lv_end_index TYPE i.
          DATA : lv_temp_cntr TYPE i.
*-------------------------------------------Check If Table is VBUK------------------------------------------------------------------
          lv_cur_row_s = <ls_remediate>-row.
          lv_cur_row_e = <ls_remediate>-row_to.

          IF <ls_remediate>-table = 'VBUK'.
            IF <ls_remediate>-multiple = abap_true.
              lv_insert = lv_insert + 1.
              lv_start_index = <ls_remediate>-row + lv_insert + lv_insert_new.   " will hold the starting index of change of code
              lv_newcode = 'DATA lt_vbuk_rem TYPE TDT_VBUK.'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'DATA lt_vbuk_range TYPE RANGE_VBELN.'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              SPLIT <ls_remediate>-value AT '@' INTO DATA(lv_forallentries_data_i) DATA(lv_field_data_i).

              IF <ls_remediate>-forallentries = abap_true.
                SPLIT <ls_remediate>-value AT '-' INTO lv_forallentries lv_field.

                lv_insert = lv_insert + 1.
                lv_newcode = 'DATA lt_vbuk_key TYPE vbuk_key_tab.'.
                INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

                lv_insert = lv_insert + 1.
                CONCATENATE 'lt_vbuk_key = CORRESPONDING #( ' lv_forallentries ' ).' INTO lv_newcode RESPECTING BLANKS.
                INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).
              ENDIF.

              lv_insert = lv_insert + 1.
              CONCATENATE 'CALL FUNCTION ' '''' 'SD_VBUK_READ_FROM_DOC_MULTI' '''' INTO lv_newcode.
*            lv_newcode = 'CALL FUNCTION ''SD_VBUK_READ_FROM_DOCMULTI'''.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'EXPORTING'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              IF <ls_remediate>-forallentries = abap_true.
                lv_insert = lv_insert + 1.
                lv_newcode = 'IT_VBUK_KEY = lt_vbuk_key' .
                INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).
              ELSE.
                lv_insert = lv_insert + 1.
                CONCATENATE 'IT_VBUK_KEY =' 'lt_vbuk_range' INTO lv_newcode SEPARATED BY space.
                INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert  + lv_insert_new ).
              ENDIF.

              lv_insert = lv_insert + 1.
              lv_newcode = 'IMPORTING'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert   + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'ET_VBUK = lt_vbuk_rem.'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              SPLIT <ls_remediate>-target AT '@' INTO lv_forallentries lv_field.
              CONCATENATE lv_forallentries '= CORRESPONDING #( lt_vbuk_rem ).' INTO lv_newcode SEPARATED BY space.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).
              lv_end_index = <ls_remediate>-row + lv_insert + lv_insert_new.     " hold the last line of the code added in the each iteration

            ELSEIF <ls_remediate>-multiple = abap_false.
              lv_insert = lv_insert + 1.
              lv_start_index = <ls_remediate>-row + lv_insert + lv_insert_new.   " will hold the starting index of change of code
              lv_newcode = 'DATA ls_vbuk_rem TYPE VBUK.'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              CONCATENATE 'CALL FUNCTION ' '''' 'SD_VBUK_READ_FROM_DOC' '''' INTO lv_newcode RESPECTING BLANKS.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'EXPORTING'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              CONCATENATE 'I_VBELN =' <ls_remediate>-value INTO lv_newcode SEPARATED BY space.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'IMPORTING'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'ES_VBUK = ls_vbuk_rem.'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              CONCATENATE <ls_remediate>-target '= CORRESPONDING #( ls_vbuk ).' INTO lv_newcode SEPARATED BY space.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).
              lv_end_index = <ls_remediate>-row + lv_insert + lv_insert_new.     " hold the last line of the code added in the each iteration
            ENDIF.
*---------------------------------------------------------------- Check if Table is VBUP---------------------------------------
          ELSEIF <ls_remediate>-table = 'VBUP'.
            IF <ls_remediate>-multiple = abap_false.
              "Handling VBUP fetch considering POSNR TBD
              lv_insert = lv_insert + 1.
              lv_start_index = <ls_remediate>-row + lv_insert + lv_insert_new.   " will hold the starting index of change of code
              lv_newcode = 'DATA ls_vbup_rem TYPE VBUP.'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              CONCATENATE 'CALL FUNCTION ' '''' 'SD_VBUP_READ_FROM_DOC' '''' INTO lv_newcode RESPECTING BLANKS.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'EXPORTING'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert  + lv_insert_new ).

              lv_insert = lv_insert + 1.
              CONCATENATE 'I_VBELN =' <ls_remediate>-value INTO lv_newcode SEPARATED BY space.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'IMPORTING'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'ES_VBUP = ls_vbup_rem.'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              CONCATENATE <ls_remediate>-target '= CORRESPONDING #( ls_vbup_rem ).' INTO lv_newcode SEPARATED BY space.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).
              lv_end_index = <ls_remediate>-row + lv_insert + lv_insert_new.     " hold the last line of the code added in the each iteration

            ELSEIF <ls_remediate>-multiple = abap_true.

              lv_insert = lv_insert + 1.
              lv_start_index = <ls_remediate>-row + lv_insert + lv_insert_new.   " will hold the starting index of change of code
              lv_newcode = 'DATA lt_vbup_remu TYPE VBUP_T.'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              IF <ls_remediate>-forallentries = abap_true.
                SPLIT <ls_remediate>-value AT '-' INTO lv_forallentries lv_field.
                SPLIT lv_forallentries AT '@' INTO DATA(lv_forallentries_data) DATA(lv_field_data).

                lv_insert = lv_insert + 1.
                lv_newcode = 'DATA lt_vbup_key TYPE vbuk_key_tab.'.
                INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

                SPLIT <ls_remediate>-value AT '-' INTO DATA(lv_forallentries_u) DATA(lv_field_u).
                SPLIT lv_forallentries_u AT '@' INTO DATA(lv_forallentries_data_u) DATA(lv_field_data_u).
                lv_insert = lv_insert + 1.
                CONCATENATE 'lt_vbup_key = CORRESPONDING #( ' lv_forallentries_data_u '  ).' INTO lv_newcode RESPECTING BLANKS.
                INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).
              ENDIF.

              lv_insert = lv_insert + 1.
              CONCATENATE 'CALL FUNCTION ' '''' 'SD_VBUP_READ_FROM_DOC_MULTI' '''' INTO lv_newcode.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'EXPORTING'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              IF <ls_remediate>-forallentries = abap_true.
                lv_insert = lv_insert + 1.
                lv_newcode = 'IT_VBUP_KEY = lt_vbup_key' .
                INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).
              ELSE.
                lv_insert = lv_insert + 1.
                CONCATENATE 'IT_VBUP_KEY =' <ls_remediate>-value INTO lv_newcode SEPARATED BY space.
                INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert  + lv_insert_new ).
              ENDIF.

              lv_insert = lv_insert + 1.
              lv_newcode = 'IMPORTING'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert  + lv_insert_new ).

              lv_insert = lv_insert + 1.
              lv_newcode = 'ET_VBUP = lt_vbup_remu.'.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).

              lv_insert = lv_insert + 1.
              CONCATENATE <ls_remediate>-target '= CORRESPONDING #( lt_vbup_remu ).' INTO lv_newcode SEPARATED BY space.
              INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert + lv_insert_new ).
              lv_end_index = <ls_remediate>-row + lv_insert + lv_insert_new.     " hold the last line of the code added in the each iteration
            ENDIF.
          ENDIF.

*----------------------------------Storing the data of before remidiation  SA_2-------------------------------------------------------
          DATA: lv_o_src(500) TYPE c,
                lv_n_src(500) TYPE c.

          LOOP AT <lfs_source>-src_code ASSIGNING FIELD-SYMBOL(<ls_src_code_vbup_vbuk>)
            FROM <ls_remediate>-row TO <ls_remediate>-row_to.
            CONCATENATE lv_o_src <ls_src_code_vbup_vbuk> INTO lv_o_src SEPARATED BY space.
          ENDLOOP.
*-------------------------------------------------SA_2----------------------------------------------------------------------------

          APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<lfs_result>).
          <lfs_result>-obj_name =   <lfs_source>-name.
          <lfs_result>-remediation_type = 'REPLACE_VBUK_VBUP'.
          <lfs_result>-before = lv_o_src.
          <lfs_result>-line_no = <ls_remediate>-row.

*----------------------------------Storing the data of after remidiation  SA_3-------------------------------------------------------

          DATA(lv_lines_add) = 0.
          LOOP AT lt_src_code ASSIGNING FIELD-SYMBOL(<ls_src_code_ch>)
            FROM <ls_remediate>-row + lv_temp_cntr TO <ls_remediate>-row_to + lv_insert.
            lv_lines_add += 1.

            CONCATENATE lv_n_src <ls_src_code_ch> INTO lv_n_src SEPARATED BY space.
          ENDLOOP.
          CALL METHOD me->set_counter
            EXPORTING
              im_obj_name  = <lfs_source>-name
              im_line_no   = <ls_remediate>-row + lv_temp_cntr
              im_lines_add = lv_lines_add.
          <lfs_result>-after = lv_n_src.
          lv_temp_cntr = lv_insert.
*----------------------------------  SA_3-------------------------------------------------------------------------------------------
          CLEAR: lv_insert_new,lv_newcode,lv_cur_row_s,lv_cur_row_e,lv_start_index,lv_end_index,lv_n_src,lv_o_src.
        ENDLOOP.
*----------------------------------------------------------------------- End of main loop----------------------------------
        CALL METHOD me->meth_validate_src_code
          EXPORTING
            im_object     = <lfs_source>-name "<ls_source_map>-name
            im_t_src_code = lt_src_code "lt_source
          IMPORTING
            ex_error      = DATA(lv_error).
        IF lv_error EQ abap_false.
          IF gv_apply_changes EQ abap_true. "SAI V2.0
            INSERT REPORT <lfs_source>-name FROM  lt_src_code.
          ENDIF.      "SAI V2.0
        ELSE.
          lv_insert = lv_insert - lv_temp_cntr.
          lt_src_code = lt_temp_src.
        ENDIF.
*----------------------------------------------------------------------
        CLEAR : lv_insert.
      ENDLOOP.                                                    "SJALLIPALL00
    ENDIF.
    "   ENDLOOP.
  ENDMETHOD.


  METHOD loop_at_new.
    CONSTANTS : lc_loop        TYPE string VALUE 'LOOP',
                lc_where       TYPE string VALUE 'WHERE',
                lc_binary_srch TYPE string VALUE '*BINARY SEARCH.',
                lc_period      TYPE string VALUE '.',
                lc_new         TYPE string VALUE 'NEW',
                lc_index       TYPE string VALUE '*INDEX*.',
                lc_sort        TYPE string VALUE 'SORT',
                lc_by          TYPE string VALUE 'BY',
                lc_eq          TYPE string VALUE '=',
                lc_info        TYPE char1 VALUE 'I',
                lc_error       TYPE char1 VALUE 'E',
                lc_success     TYPE char1 VALUE 'S'.

    DATA : lr_scan        TYPE REF TO cl_ci_scan,
           lv_line        TYPE i,
           lv_counter     TYPE i,
           ls_word        TYPE string,
           lv_pgm_name    TYPE string,
           ls_dir         TYPE trdir,
           ls_msg         TYPE string,
           lt_warnings    TYPE synt_errors,
           lv_fld         TYPE string,
           lt_source_map  TYPE tt_source_map,
           lt_source_code TYPE string_table,
           lv_curr_cntr   TYPE i.

    DATA: lt_lines TYPE STANDARD TABLE OF i.

    IF gv_apply_changes IS INITIAL.
      "Code Scan
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = im_object
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = DATA(lt_source_map_temp).

    ELSE.
      lt_source_map_temp = VALUE #( FOR lwa_remed IN gt_remediation WHERE  ( remediation_type = 'LOOP AT NEW' )
                                                    ( name = lwa_remed-obj_name )  ).
 SORT lt_source_map_temp by name.
      DELETE ADJACENT DUPLICATES FROM lt_source_map_temp COMPARING name.
    ENDIF.

    LOOP AT lt_source_map_temp ASSIGNING FIELD-SYMBOL(<lfs_smap>).
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = <lfs_smap>-name
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = lt_source_map.
**-->Tokens will store all the keywords including nested includes too.
      "Since it will have all the tokens copy the current level tokens only.
      DATA(lv_level) = lt_source_map[ name = <lfs_smap>-name ]-level.
      DATA(lt_temp_stmnts) = lr_scan->statements.

      SORT lt_temp_stmnts BY level from.
      DATA(lv_token_from) = lt_temp_stmnts[ level = lv_level ]-from.

      SORT lt_temp_stmnts BY level ASCENDING from DESCENDING.
      DATA(lv_token_to) =  lt_temp_stmnts[ level = lv_level ]-to.


      IF gv_apply_changes EQ abap_true.
        CLEAR lt_lines.
        lt_lines = VALUE #( FOR lwa_remed IN gt_remediation WHERE  (
        obj_name = <lfs_smap>-name AND
        remediation_type = 'LOOP AT NEW'    )
           ( lwa_remed-line_no )  ).
      ENDIF.
      "Fetch statements with LOOP query
      LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_scan>) WHERE str = lc_loop.
        DATA(lv_index) = sy-tabix.

        IF lv_index > lv_token_to.
          EXIT.
        ENDIF.

        DATA(lv_row_read) = <ls_scan>-row.

        READ TABLE lr_scan->tokens INTO DATA(ls_token) INDEX lv_index + 1.
        IF sy-subrc = 0 AND ls_token-str = 'AT'.
********* Read int_tab name from LOOP statement.
          READ TABLE lr_scan->tokens INTO DATA(ls_tok_scan) INDEX lv_index + 2.
          IF sy-subrc EQ 0.
            DATA(lv_sort1) = | { lc_sort } { ls_tok_scan-str } { lc_by } |.
          ENDIF.
          READ TABLE lr_scan->statements ASSIGNING FIELD-SYMBOL(<lfs_keys>)
                                                  WITH KEY from = lv_index.
          IF sy-subrc EQ 0.
            READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_from>) INDEX <lfs_keys>-from.
            IF sy-subrc EQ 0.
              DATA(lv_src_line_from) = <ls_token_from>-row.
            ENDIF.
            READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_to>) INDEX <lfs_keys>-to.
            IF sy-subrc EQ 0.
              DATA(lv_src_line_to) = <ls_token_to>-row.
            ENDIF.
            IF gv_apply_changes EQ abap_true.
              CALL METHOD me->get_counter
                EXPORTING
                  im_obj_name = <lfs_smap>-name
                  im_line_no  = lv_src_line_from
                IMPORTING
                  ex_counter  = DATA(lv_g_cntr).
              lv_g_cntr = lv_g_cntr - lv_counter.
              READ TABLE lt_lines TRANSPORTING NO FIELDS WITH  KEY  table_line = lv_src_line_from - lv_g_cntr."<lfs_glb_cntr>-counter.
              IF sy-subrc NE 0.
                CONTINUE.
              ENDIF.
            ENDIF.
            LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<fs_tokens>) WHERE str = 'AT'.
              DATA(lv_tabix) = sy-tabix.
              lv_tabix = lv_tabix + 1.
              READ TABLE lr_scan->tokens INTO ls_token INDEX lv_tabix.
              IF sy-subrc = 0 AND ls_token-str = 'NEW'.
                DATA(lv_sort2) = lv_sort1.
*              DATA(lv_row_count) = sy-index.
                DATA(lv_idx) = lv_tabix + 1.
                lv_tabix = lv_tabix + 2.
                READ TABLE lr_scan->tokens INTO DATA(ls_tokens_temp) INDEX lv_idx.
                IF sy-subrc EQ 0.

                  lv_fld = |{ lv_fld } { ls_tokens_temp-str }|.

                ENDIF.
              ENDIF.
            ENDLOOP.
            IF lv_fld IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.

          CONDENSE lv_fld.
          "Generate SORT statement
          lv_sort2 = |{ lv_sort2 }{ lv_fld } .|.
          CONDENSE lv_sort2.
          "Get end line of one statement
          LOOP AT lr_scan->statements ASSIGNING FIELD-SYMBOL(<ls_statements>)
                                                WHERE from <= lv_index AND
                                                      to   >= lv_index.
            lv_level = <ls_statements>-level.
            DATA(lv_row) = <ls_statements>-trow.
            EXIT.
          ENDLOOP.


          "Place binary search in read statement
          READ TABLE lt_source_map ASSIGNING FIELD-SYMBOL(<ls_source_map>) WITH KEY level = lv_level.
          IF sy-subrc = 0 .
            IF lv_pgm_name <> <ls_source_map>-name.
              DATA(lt_source) = <ls_source_map>-src_code.
              CLEAR lv_counter.
            ENDIF.
            DATA(lt_temp_src) = lt_source.
            lv_pgm_name = <ls_source_map>-name.

            READ TABLE lt_source ASSIGNING FIELD-SYMBOL(<ls_src_code>) INDEX lv_row.
            IF sy-subrc = 0.
**Check whether Sorting is Done Already
              DATA(lv_temp_cntr) = lv_row_read.
              DATA: lv_temp_code TYPE string.
              DO 3 TIMES.
                READ TABLE <ls_source_map>-src_code
                        ASSIGNING <ls_src_code> INDEX lv_temp_cntr.

                lv_temp_code = |{ <ls_src_code> } { lv_temp_code }|.
                lv_temp_cntr = lv_temp_cntr - 1.
              ENDDO.

              CONDENSE lv_temp_code.
              TRANSLATE lv_temp_code TO UPPER CASE.
              TRANSLATE lv_sort2 TO UPPER CASE.

              IF NOT ( lv_temp_code CS lv_sort2 ).
                INSERT |{ lv_sort2 } { gv_comment }|
                       INTO lt_source INDEX lv_counter + lv_row_read.
                lv_counter = lv_counter + 1.
                lv_curr_cntr += 1.
                CALL METHOD me->set_counter
                  EXPORTING
                    im_obj_name  = <lfs_smap>-name
                    im_line_no   = lv_row_read
                    im_lines_add = lv_curr_cntr.
              ENDIF.
              CLEAR: lv_temp_cntr, lv_temp_code.
              ev_insert = abap_true.
            ENDIF.
          ENDIF.
          CLEAR : lv_sort2,
                  lv_fld,
                  "lv_key_ind,
                  ls_tok_scan,
                  ls_tokens_temp,
                  ls_msg,
                  lv_line.
        ENDIF.



*Syntax Check for the updated program : Main program

        IF sy-subrc EQ 0.
          IF gv_apply_changes EQ abap_false.
**--Begin of SJALLIPALL00
            DATA: lv_o_src TYPE String,
                  lv_n_src TYPE string.

            LOOP AT <ls_source_map>-src_code ASSIGNING <ls_src_code>
              FROM lv_src_line_from TO lv_src_line_to.
              CONCATENATE lv_o_src <ls_src_code> INTO lv_o_src SEPARATED BY space.
*            APPEND <ls_src_code> TO lt_old_source_code.
            ENDLOOP.

**--BEgin of V2.0
            APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<lfs_result>).
            <lfs_result>-obj_name =   <ls_source_map>-name.
            <lfs_result>-remediation_type = 'LOOP AT NEW'.
            <lfs_result>-before = lv_o_src.
            <lfs_result>-line_no = lv_src_line_from.
**--End of V2.0

*          DATA(lv_org_lines) = lines( lt_old_source_code ).
            lv_temp_cntr = lv_counter - 1.
            LOOP AT lt_source ASSIGNING <ls_src_code>
              FROM lv_src_line_from + lv_temp_cntr TO lv_src_line_to + lv_counter.
              CONCATENATE lv_n_src <ls_src_code> INTO lv_n_src SEPARATED BY space.
            ENDLOOP.
            <lfs_result>-after = lv_n_src.
**--End of SJALLIPALL00
          ENDIF.
          CALL METHOD me->meth_validate_src_code
            EXPORTING
              im_object     = <ls_source_map>-name
              im_t_src_code = lt_source
            IMPORTING
              ex_error      = DATA(lv_error).
          IF lv_error EQ abap_false.
            IF gv_apply_changes EQ abap_true. "SAI V2.0
              INSERT REPORT <ls_source_map>-name FROM
              lt_source.
              "<lfs_glb_cntr>-counter = <lfs_glb_cntr>-counter + lv_counter.
            ENDIF.      "SAI V2.0
          ELSE.
            lv_counter = lv_counter - lv_curr_cntr.
            lt_source = lt_temp_src.
          ENDIF.
**--End of SJALLIPALL00
        ENDIF.

        CLEAR : lv_sort2,
            lv_fld,
            ls_tok_scan,
            ls_tokens_temp,
            ls_msg,
            lv_line,
            lv_o_src,
            lv_n_src,
            lv_curr_cntr.
      ENDLOOP.
      CLEAR lv_counter.
    ENDLOOP.
 me->adjust_counter( ).
  ENDMETHOD.


  METHOD generate_program.

    DATA: lv_msg TYPE text240,
          conv   TYPE REF TO cl_wb_abap_source_format.
    CONSTANTS: lc_success TYPE char1 VALUE 'S',
               lc_abap    TYPE char4 VALUE 'ABAP'.

   DATA(lv_object) = COND #( WHEN gv_obj_type = 'FUNC' THEN GV_FUGR_NAME
                             ELSE gv_object_name ).
   DATA(lv_obj_class) = COND #( WHEN gv_obj_type = 'CLAS' THEN gv_obj_type
                             ELSE 'ABAP' ).

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = lv_object"is_obj_details-program_name
        object_class        = lv_obj_class"gv_obj_type"lc_abap
        korrnum             = is_obj_details-transport_number
        mode                = 'MODIFY'
        suppress_dialog     = abap_true
        activation_call     = abap_true
        devclass            = 'ZTEST_CRT_TOOL'
        author              = sy-uname
        global_lock         = abap_true "Capture to entire object
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = sy-msgid
          lang      = sy-langu
          no        = sy-msgno
          v1        = sy-msgv1
          v2        = sy-msgv2
          v3        = sy-msgv3
          v4        = sy-msgv4
        IMPORTING
          msg       = lv_msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO ex_message ASSIGNING FIELD-SYMBOL(<lfs_message>).
        <lfs_message>-typ = sy-msgty.
        <lfs_message>-message = lv_msg.
      ENDIF.
    ELSE.
      APPEND INITIAL LINE TO ex_message ASSIGNING <lfs_message>.
      <lfs_message>-typ = lc_success.
      <lfs_message>-message = TEXT-002.
    ENDIF.

    CLEAR: lv_msg.

  ENDMETHOD.


  METHOD execute.

    DATA : lt_messages        TYPE tt_message,
           lv_insert          TYPE flag,
           lv_code_remediated TYPE flag,
           ls_obj_details     TYPE ty_obj_details,
           lr_conv            TYPE REF TO cl_wb_abap_source_format,
           lt_src_code        TYPE TABLE OF string,
           lv_obj_typ         TYPE trobjtype.

    TYPES: BEGIN OF lty_remediation,
             remediation_type TYPE string,
           END OF lty_remediation.


    DATA : lt_remediations TYPE TABLE OF lty_remediation.

    IF im_apply_changes EQ abap_false..
      lt_remediations = VALUE #(

                                 ( remediation_type = 'READ_WRITE_ON_DB_REMEDIATION')
                                 ( remediation_type = 'READ REMEDIATION')
                                 ( remediation_type = 'DELETE DUPLICATE')
                                 ( remediation_type = 'LOOP AT NEW')
                                 ( remediation_type = 'FOR ALL ENTRIES')
                                 ( remediation_type = 'REPLACE_VBUK_VBUP')
                                 ( remediation_type = 'SELECT SINGLE') "MATNR pending
                               ).
    ELSE.
      gt_remediation = im_cnfrm_remed.
      lt_remediations = VALUE #( FOR lwa_remediation IN im_cnfrm_remed
                                        ( remediation_type = lwa_remediation-remediation_type ) ).
      SORT lt_remediations BY remediation_type.
      DELETE ADJACENT DUPLICATES FROM lt_remediations COMPARING remediation_type.
    ENDIF.

**--Begin of SJALLIPALL00 V2.0
    gv_apply_changes = im_apply_changes.
**--End of SJALLIPALL00 V2.0
    IF im_object+0(1) = 'Z'.
**--Begin of SJALLIPALL00
**--Based on the Object name and object type
      "Fetch the original repository obj for remediation
      "Such as based on class name it will fetch the class pool include which contsains the logic
      "Based on FM name or FG it will return the Function group source code include
      "Which we can remediate all the functions with in the function group
      "Fetch remediate object name
      CALL METHOD me->meth_fetch_remediate_obj
        EXPORTING
          im_object          = im_object
        IMPORTING
          ex_rem_obj         = DATA(lv_rem_obj)
          ex_error           = DATA(lv_error)
          ex_set_prety_print = DATA(lv_set_pp)
        CHANGING
          ch_objtyp          = lv_obj_typ.
**--End of SJALLIPALL00
      "Call Read remediation method
      LOOP AT lt_remediations ASSIGNING FIELD-SYMBOL(<lfs_remed>).
        CASE <lfs_remed>-remediation_type.

          WHEN  'READ REMEDIATION'.
            CALL METHOD me->read_remediation
              EXPORTING
                im_object    = lv_rem_obj "im_object
                im_objtyp    = lv_obj_typ "im_objtyp
                im_runseries = im_runseries
              IMPORTING
                ev_insert    = lv_insert
                ex_message   = lt_messages.

            APPEND LINES OF lt_messages TO ex_message.
            CLEAR : lt_messages.

            "Check if Code has been remediated
            IF lv_code_remediated = abap_false.
              IF lv_insert = abap_true.
                lv_code_remediated = abap_true.
              ENDIF.
            ENDIF.
          WHEN 'DELETE DUPLICATE'.
            "Call delete_duplicate remediation method
            CALL METHOD me->delete_duplicate
              EXPORTING
                im_object    = lv_rem_obj "im_object
                im_objtyp    = lv_obj_typ "im_objtyp
                im_runseries = im_runseries
              IMPORTING
                ev_insert    = lv_insert
                ex_message   = lt_messages.

            APPEND LINES OF lt_messages TO ex_message.
            CLEAR : lt_messages.
*
            "Check if Code has been remediated
            IF lv_code_remediated = abap_false.
              IF lv_insert = abap_true.
                lv_code_remediated = abap_true.
              ENDIF.
            ENDIF.

            "Call loop remedi ation method "COmmented by SAI need clarification..
*        CALL METHOD me->loop_remediation
*          EXPORTING
*            im_object    = lv_rem_obj "im_object
*            im_objtyp    = lv_obj_typ "im_objtyp
*            im_runseries = im_runseries
*          IMPORTING
*            ev_insert    = lv_insert
*            ex_message   = lt_messages.
*
*        APPEND LINES OF lt_messages TO ex_message.
*        CLEAR : lt_messages.
*
*        "Check if Code has been remediated
*        IF lv_code_remediated = abap_false.
*          IF lv_insert = abap_true.
*            lv_code_remediated = abap_true.
*          ENDIF.
*        ENDIF.
*
          WHEN 'LOOP AT NEW'.
            "Call LOOP_AT_NEW remediation method
            CALL METHOD me->loop_at_new
              EXPORTING
                im_object    = lv_rem_obj
                im_objtyp    = lv_obj_typ
                im_runseries = im_runseries
              IMPORTING
                ev_insert    = lv_insert
                ex_message   = lt_messages.

            APPEND LINES OF lt_messages TO ex_message.
            CLEAR : lt_messages.

            "Check if Code has been remediated
            IF lv_code_remediated = abap_false.
              IF lv_insert = abap_true.
                lv_code_remediated = abap_true.
              ENDIF.
            ENDIF.
*
          WHEN 'SELECT SINGLE'.
            "Select Single remedian method
            CALL METHOD me->select_single_remediation
              EXPORTING
                im_object    = lv_rem_obj
                im_runseries = im_runseries
              IMPORTING
                ev_insert    = lv_insert
                ex_message   = lt_messages.

            APPEND LINES OF lt_messages TO ex_message.
            CLEAR : lt_messages.

            "Check if Code has been remediated
            IF lv_code_remediated = abap_false.
              IF lv_insert = abap_true.
                lv_code_remediated = abap_true.
              ENDIF.
            ENDIF.
          WHEN 'READ_WRITE_ON_DB_REMEDIATION'.
*    "Read and Write remedian method
            CALL METHOD me->read_write_on_db_remediation
              EXPORTING
                im_object    = lv_rem_obj "im_object
                im_runseries = im_runseries
              IMPORTING
                ev_insert    = lv_insert
                ex_message   = lt_messages.

            APPEND LINES OF lt_messages TO ex_message.
            CLEAR : lt_messages.

*    Check if Code has been remediated
            IF lv_code_remediated = abap_false.
              IF lv_insert = abap_true.
                lv_code_remediated = abap_true.
              ENDIF.
            ENDIF.
          WHEN 'FOR ALL ENTRIES'.
*    for all remedian method
            CALL METHOD me->for_all_entries_remediation
              EXPORTING
                im_object    = lv_rem_obj "im_object
                im_runseries = im_runseries
              IMPORTING
                ev_insert    = lv_insert
                ex_message   = lt_messages.

            APPEND LINES OF lt_messages TO ex_message.
            CLEAR : lt_messages.

*      Check if Code has been remediated
            IF lv_code_remediated = abap_false.
              IF lv_insert = abap_true.
                lv_code_remediated = abap_true.
              ENDIF.
            ENDIF.
          WHEN 'REPLACE_VBUK_VBUP'.
            "Replace VBUK & VBUP method
            CALL METHOD me->replace_vbuk_vbup
              EXPORTING
                im_object    = lv_rem_obj "im_object
                im_runseries = im_runseries
              IMPORTING
                ev_insert    = lv_insert
                ex_message   = lt_messages.

            APPEND LINES OF lt_messages TO ex_message.
            CLEAR : lt_messages.
          WHEN 'MATNR'.
            IF 1 = 2. " For testing purpose SJALLIPALL00 V2.0
              CALL METHOD me->replace_matnr_de
                EXPORTING
                  im_object    = lv_rem_obj "im_object
                  im_runseries = im_runseries
                IMPORTING
                  ev_insert    = lv_insert
                  ex_message   = lt_messages.

              APPEND LINES OF lt_messages TO ex_message.
              CLEAR : lt_messages.

*      Check if Code has been remediated
              IF lv_code_remediated = abap_false.
                IF lv_insert = abap_true.
                  lv_code_remediated = abap_true.
                ENDIF.
              ENDIF.
            ENDIF. "For testing purpose SJALLIPALL00 V2.0
        ENDCASE.
      ENDLOOP.

      IF  gv_apply_changes EQ abap_true..
***--Code Wrap at 72 characters
        CALL METHOD me->code_wrap
          EXPORTING
            im_object = lv_rem_obj.

        CLEAR ex_message.
        IF lv_set_pp EQ abap_true.
          CALL METHOD me->process_pretty_printer
            EXPORTING
              im_object    = lv_rem_obj
              im_runseries = im_runseries
            IMPORTING
              ev_insert    = lv_insert
              ex_message   = lt_messages.

          APPEND LINES OF lt_messages TO ex_message.
          CLEAR : lt_messages.
        ENDIF.


        me->meth_get_delta_changes(
              EXPORTING
                im_object = lv_rem_obj ).

        CALL METHOD me->process_application_log
          EXPORTING
            im_object  = lv_rem_obj
            ex_message = ex_message.


        CALL METHOD me->get_obj_detail
          EXPORTING
            im_object      = im_object
            im_transportno = im_transportno
            im_package     = im_package
          IMPORTING
            es_obj_details = ls_obj_details.

        CALL METHOD me->generate_program
          EXPORTING
            is_obj_details = ls_obj_details
          IMPORTING
            ex_message     = lt_messages.

        APPEND LINES OF lt_messages TO ex_message.
        CLEAR : lt_messages.
      ENDIF.
      SORT ex_message.
      DELETE ADJACENT DUPLICATES FROM ex_message COMPARING ALL FIELDS .


    .ENDIF.

    ex_result  = gt_result.
    IF sy-uname = 'SJALLIPALL00'.
      gt_remediation = gt_result.
      gv_apply_changes = abap_true.
      CLEAR: gt_result, gt_obj_cntr,gt_obj_cntr_temp.
    ENDIF.

  ENDMETHOD.


  METHOD add_comments.
    DATA lv_date TYPE char10.

    WRITE sy-datum TO lv_date.
    "Add Comments
    re_comment = |{ text-001 } { sy-uname } { text-003 } { lv_date }|.
  ENDMETHOD.


  METHOD adjust_counter.
*    DATA : lt_temp_tab  TYPE tt_obj_counter,
*           lt_temp_tab2 TYPE tt_obj_counter.
*    lt_temp_tab = gt_obj_cntr_temp.
*    lt_temp_tab2 = gt_obj_cntr_temp.
*
*    LOOP AT gt_obj_cntr_temp INTO DATA(lwa_temp) WHERE sequence LT gv_seq.
*      DATA(lv_count) = 0.
*      DATA(lv_index) =  sy-tabix.
*      LOOP AT lt_temp_tab2 INTO DATA(lwa_temp2) WHERE at_line LT lwa_temp-at_line
*        AND sequence = gv_seq.
*        lv_count += lwa_temp2-counter.
*      ENDLOOP.
*      READ TABLE lt_temp_tab ASSIGNING FIELD-SYMBOL(<lfs_adjust>) INDEX lv_index.
*      IF sy-subrc EQ 0.
*        <lfs_adjust>-at_line += lv_count.
*      ENDIF.
*
*    ENDLOOP.
*
*    CLEAR : gt_obj_cntr.
*    gt_obj_cntr = lt_temp_tab.

  ENDMETHOD.


  METHOD for_all_entries_remediation.
*--Data Declaration--*
    DATA: gr_source     TYPE REF TO cl_ci_source_include,
          lv_count      TYPE i,
          lt_source_map TYPE TABLE OF ty_source_map,
          gt_tokens     TYPE stokesx_tab.

    DATA: gv_tabix        TYPE sy-tabix,
          gv_tabname      TYPE dcobjdef-name,
          gt_keyfield     TYPE STANDARD TABLE OF cacs_s_cond_keyfields,
          gv_allkey       TYPE c VALUE 'X',
          gv_counter      TYPE i,
          gv_program_name TYPE string.

    CONSTANTS : lc_info    TYPE char1 VALUE 'I',
                lc_error   TYPE char1 VALUE 'E',
                lc_success TYPE char1 VALUE 'S'.

    DATA: lt_n_src_code      TYPE string_table,
          lt_o_src_code      TYPE string_table,
          lv_n_src           TYPE string,
          lv_o_src           TYPE string,
          lv_counter         TYPE i,
          lt_remediation_obj TYPE SORTED TABLE OF zstr_code_remediation
                                  WITH NON-UNIQUE KEY remediation_type.
    DATA: lt_lines TYPE STANDARD TABLE OF i.

    gv_seq += 1.

    IF gv_apply_changes IS INITIAL.
      "Code Scan
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = im_object
        IMPORTING
          ex_scan       = DATA(lr_scan)
          et_source_map = DATA(lt_source_map_temp).

    ELSE.
      lt_source_map_temp = VALUE #( FOR lwa_remed IN gt_remediation WHERE  ( remediation_type = 'FOR_ALL_ENTRIES_REMEDIATION' )
                                                    ( name = lwa_remed-obj_name )  ).
      SORT lt_source_map_temp by name.
      DELETE ADJACENT DUPLICATES FROM lt_source_map_temp COMPARING name.
    ENDIF.

*Fetch statements with FOR ALL ENTRIES


    LOOP AT lt_source_map_temp ASSIGNING FIELD-SYMBOL(<lfs_smap>).
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = <lfs_smap>-name
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = lt_source_map.
      IF gv_apply_changes EQ abap_true.
        CLEAR lt_lines.

        lt_lines = VALUE #( FOR lwa_remed IN gt_remediation WHERE  (
        obj_name = <lfs_smap>-name AND
        remediation_type = 'FOR_ALL_ENTRIES_REMEDIATION'    )
           ( lwa_remed-line_no )  ).
      ENDIF.
      LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_scan>) WHERE str = 'FOR'.
        DATA(lv_index) = sy-tabix.
        DATA(lv_row_read) = <ls_scan>-row.

        READ TABLE lr_scan->tokens INTO DATA(ls_token) INDEX lv_index + 1.
        IF sy-subrc = 0 AND ls_token-str = 'ALL'.
          READ TABLE lr_scan->tokens INTO ls_token INDEX lv_index + 2.
          IF sy-subrc = 0 AND ls_token-str = 'ENTRIES'.
            READ TABLE lr_scan->tokens INTO ls_token INDEX lv_index + 4.
            IF sy-subrc = 0.
              DATA(l_tab_name) = ls_token-str. "Table Name.
              SHIFT l_tab_name LEFT DELETING LEADING '@'.
              CONDENSE l_tab_name.
              DATA(l_tab_b) = |{ l_tab_name }[]|.

*Code Strings to Check if Validation is done alreay
              DATA(lv_check_str1) = |IF { l_tab_b } IS NOT INITIAL.|.
              DATA(lv_check_str2) = |IF { l_tab_name } IS NOT INITIAL.|.
              DATA(lv_check_str3) = |IF LINES( { l_tab_name } ) GT 0|.
              DATA(lv_check_str4) = |IF LINES( { l_tab_name } ) > 0|.
              DATA(lv_check_str5) = |IF LINES( { l_tab_b } ) GT 0|.
              DATA(lv_check_str6) = |IF LINES( { l_tab_b } ) > 0|.

              TRANSLATE: lv_check_str1 TO UPPER CASE,
                         lv_check_str2 TO UPPER CASE,
                         lv_check_str3 TO UPPER CASE,
                         lv_check_str4 TO UPPER CASE,
                         lv_check_str5 TO UPPER CASE,
                         lv_check_str6 TO UPPER CASE.

              "Get end line of one statement
              LOOP AT lr_scan->statements
                                 ASSIGNING FIELD-SYMBOL(<ls_statements>)
                                                  WHERE from <= lv_index
                                                    AND to   >= lv_index.
                DATA(lv_level) = <ls_statements>-level.
                DATA(lv_row)   = <ls_statements>-trow + gv_counter .
                DATA(lv_from)  = <ls_statements>-from.
                DATA(lv_to)    = <ls_statements>-to.
                EXIT.
              ENDLOOP.
**---V2.0
              READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_from>) INDEX lv_from.
              IF sy-subrc EQ 0.
                DATA(lv_src_line_from) = <ls_token_from>-row.
              ENDIF.
              READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_to>) INDEX lv_to.
              IF sy-subrc EQ 0.
                DATA(lv_src_line_to) = <ls_token_to>-row.
              ENDIF.
              IF gv_apply_changes EQ abap_true.
                CALL METHOD me->get_counter
                  EXPORTING
                    im_obj_name = <lfs_smap>-name
                    im_line_no  = lv_src_line_from
                  IMPORTING
                    ex_counter  = DATA(lv_g_cntr).
                "lv_g_cntr = lv_g_cntr - gv_counter.
                READ TABLE lt_lines TRANSPORTING NO FIELDS WITH
                  KEY  table_line = lv_src_line_from - lv_g_cntr."<lfs_glb_cntr>-counter.
                IF sy-subrc NE 0.
                  CONTINUE.
                ENDIF.
              ENDIF.
**--V2.0
              READ TABLE lt_source_map ASSIGNING FIELD-SYMBOL(<ls_source_map>)
                                         WITH KEY level = lv_level.
              IF sy-subrc = 0.

                IF gv_program_name <> <ls_source_map>-name.
                  DATA(lt_src_code) = <ls_source_map>-src_code.
                  CLEAR : gv_counter.
                ENDIF.
                DATA(lt_temp_src) = lt_src_code.
                DATA(lv_curr_cntr) = 0.
                gv_program_name = <ls_source_map>-name.

                DATA(lv_row_temp) = lv_row + gv_counter .
                IF sy-subrc = 0.
                  DO.
                    READ TABLE lt_src_code  "<ls_source_map>-src_code
                            ASSIGNING FIELD-SYMBOL(<ls_src_code>) INDEX lv_row_temp.

                    "Check if entire statment is in same line without Break
                    IF <ls_src_code> CS 'SELECT'.
*--------Begin of Block to Check if Validation is Done already---------*
                      DATA(lv_validation_row) = lv_row_temp.
                      DATA: lv_temp_code TYPE string.

*Get Code before the Select Query
                      DO 4 TIMES.
                        READ TABLE <ls_source_map>-src_code
                                   ASSIGNING FIELD-SYMBOL(<ls_src_code2>)
                                   INDEX LV_validation_row.
                        lv_temp_code = |{ <ls_src_code2> } { lv_temp_code }|.
                        LV_validation_row = lv_validation_row - 1.
                      ENDDO.

                      CONDENSE lv_temp_code.
                      TRANSLATE lv_temp_code TO UPPER CASE.
*Check if Validation Code exists in different combinations
                      IF lv_temp_code CS lv_check_str1 OR
                         lv_temp_code CS lv_check_str2 OR
                         lv_temp_code CS lv_check_str3 OR
                         lv_temp_code CS lv_check_str4 OR
                         lv_temp_code CS lv_check_str5 OR
                         lv_temp_code CS lv_check_str6.
                        DATA(l_validated) = abap_true.
                        EXIT.
                      ENDIF.
*---------End of Block to Check if Validation is Done already----------*

*Add Code if Validation not Done
*                <ls_src_code> = |{ lv_check_str1 } { <ls_src_code> } { gv_code_with_comment }|.
                      IF l_validated <> abap_true.

                        INSERT |{ lv_check_str1 } { gv_comment }|   INTO lt_src_code INDEX lv_row_temp." + gv_counter.
                        lv_curr_cntr += 1.
                        CALL METHOD me->set_counter
                          EXPORTING
                            im_obj_name  = <lfs_smap>-name
                            im_line_no   = lv_row_temp
                            im_lines_add = 1.
*                      gv_counter += 1.
*                    INSERT REPORT <ls_source_map>-name
*                         FROM lt_src_code.
                        DATA(lv_insert) = abap_true.
                        EXIT.
                      ENDIF.
                    ELSE.
                      lv_row_temp = lv_row_temp - 1.
                    ENDIF.
                  ENDDO.

*Add Endif for the Above IF
                  IF l_validated <> abap_true.
                    INSERT |ENDIF. { gv_comment } |
                         INTO lt_src_code INDEX lv_row + 2 ."+ gv_counter.
                    lv_curr_cntr += 1.
                    CALL METHOD me->set_counter
                      EXPORTING
                        im_obj_name  = <lfs_smap>-name
                        im_line_no   = lv_row + 2
                        im_lines_add = 1.
                    DATA(lv_n_line_to) =   lv_row + 2 + gv_counter.
                    IF gv_apply_changes EQ abap_false.
*                      DATA: lv_o_src TYPE string,
*                            lv_n_src TYPE string.
                      LOOP AT <ls_source_map>-src_code ASSIGNING <ls_src_code>
                        FROM lv_src_line_from TO lv_src_line_to.
                        CONCATENATE lv_o_src <ls_src_code> INTO lv_o_src SEPARATED BY space.
                      ENDLOOP.
                      LOOP AT lt_src_code ASSIGNING <ls_src_code>
                         FROM lv_src_line_from  TO lv_n_line_to.
                        CONCATENATE lv_n_src <ls_src_code> INTO lv_n_src SEPARATED BY space.
                      ENDLOOP.
                      APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<lfs_result>).
                      <lfs_result>-obj_name = <ls_source_map>-name.
                      <lfs_result>-remediation_type  = 'FOR_ALL_ENTRIES_REMEDIATION'.
                      <lfs_result>-after =  lv_n_src.
                      <lfs_result>-before = lv_o_src.
                      <lfs_result>-line_no = lv_src_line_from.
                    ENDIF.


                    CALL METHOD me->meth_validate_src_code
                      EXPORTING
                        im_object     = <ls_source_map>-name
                        im_t_src_code = lt_src_code
                      IMPORTING
                        ex_error      = DATA(lv_error).
                    IF lv_error EQ abap_false AND gv_apply_changes EQ abap_true.
                      INSERT REPORT <ls_source_map>-name
                            FROM lt_src_code.
                      gv_counter = gv_counter + 2.
                    ELSE.
                      lt_src_code = lt_temp_src.
                    ENDIF.
                  ENDIF.
                  CLEAR: l_validated, lv_temp_code.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      ev_insert = lv_insert.
      CLEAR: lv_row_temp, gv_counter.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_instance.

    IF go_instance IS NOT BOUND.
      CREATE OBJECT go_instance.
    ENDIF.
*
    ro_instance = go_instance .

  ENDMETHOD.


  METHOD process_pretty_printer.
    DATA: lt_src_code TYPE TABLE OF string.
    DATA:lc_x TYPE char1 .
    DATA: lv_lines      TYPE i,
          "Variable to capture error message
          lv_line       TYPE i,
          ls_word       TYPE string,
          ls_dir        TYPE trdir,
          ls_msg        TYPE string,
          lt_warnings   TYPE synt_errors,
          lv_flag       TYPE c,
          lv_old_code   TYPE string,
          lv_first_line TYPE sy-tabix.

    CONSTANTS:lc_success TYPE char1 VALUE 'S'.
    CLEAR ex_message.
    READ REPORT im_object INTO lt_src_code.

    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo             = 'X'
      TABLES
        otext              = lt_src_code
        ntext              = lt_src_code
      EXCEPTIONS
        include_enqueued   = 1
        include_readerror  = 2
        include_writeerror = 3
        enqueue_table_full = 4.

    IF sy-subrc NE 0.

      "All OK
    ENDIF.


** Update the line with new code.
    INSERT REPORT im_object FROM lt_src_code.


*    ** Syntax Check for the updated program
    CALL METHOD me->prog_syntax_check
      EXPORTING
        im_object  = im_object
        im_srccode = lt_src_code
*       im_srccode = <ls_source_map>-src_code
      IMPORTING
        et_warning = lt_warnings
        ex_line    = lv_line
        ex_word    = ls_word
        ex_msg     = ls_msg
        ex_dir     = ls_dir.
*                  IF lv_line NE 0 AND ls_msg IS NOT INITIAL.
    IF lt_warnings IS NOT INITIAL.
      LOOP AT lt_warnings ASSIGNING FIELD-SYMBOL(<fs_warning>).
        APPEND INITIAL LINE TO ex_message ASSIGNING FIELD-SYMBOL(<lfs_message>).
        <lfs_message>-line = <fs_warning>-line.
        <lfs_message>-message = <fs_warning>-message.
        <lfs_message>-typ = 'E'.
      ENDLOOP.
      CLEAR : ev_insert.
    ELSE.
      ev_insert = abap_true.
      APPEND INITIAL LINE TO ex_message ASSIGNING <lfs_message>.
      <lfs_message>-typ = 'S'.
      <lfs_message>-message = TEXT-000.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.


  METHOD replace_matnr_de.
**--Begin of      "SJALLIPALL00
    "Declaring Local type
    TYPES: BEGIN OF lty_remediate,
             level TYPE int4.
             INCLUDE TYPE zcl_code_remediation=>ty_remediate.
    TYPES: END OF lty_remediate.
**--End of      "SJALLIPALL00

    "Declaring global variables
    DATA: gr_scan       TYPE REF TO cl_ci_scan,
          gr_source     TYPE REF TO cl_ci_source_include,
          gr_source_map TYPE tt_source_map,
          gt_comments   TYPE tt_comments.

    "Declare internal table and work area
    DATA: gt_token TYPE stokesx_tab.
    DATA: gt_token1 TYPE stokesx_tab.
    DATA: gs_token TYPE stokesx.

    DATA: lt_src_code TYPE TABLE OF string.
    DATA: lt_src TYPE TABLE OF string.
    DATA: lt_n_src_code TYPE string_table,
          lt_o_src_code TYPE string_table.


    "Decalring local variable
    DATA:lc_x TYPE char1 .
    DATA: lv_lines      TYPE i,
          "Variable to capture error message
          lv_line       TYPE i,
          ls_word       TYPE string,
          ls_dir        TYPE trdir,
          ls_msg        TYPE string,
          lt_warnings   TYPE synt_errors,
          lv_flag       TYPE c,
          lv_old_code   TYPE string,
          lv_first_line TYPE sy-tabix.

    DATA: lv_string        TYPE string,
          lv_newcode       TYPE string,
          lv_forallentries TYPE string,
          lv_field         TYPE string,
          ls_remediate     TYPE lty_remediate, "me->ty_remediate,                           "SJALLIPALL00
          lt_remediate     TYPE STANDARD TABLE OF lty_remediate. "me->ty_remediate.         "SJALLIPALL00

    CONSTANTS:lc_success TYPE char1 VALUE 'S'.

    "Code Scan
    CALL METHOD me->code_scan
      EXPORTING
        im_object     = im_object
      IMPORTING
        ex_scan       = gr_scan
        et_source_map = gr_source_map.

    DATA(lt_stmnt_tmp) =  gr_scan->statements.

    LOOP AT gr_scan->statements INTO DATA(ls_stat).

      READ TABLE gr_scan->levels ASSIGNING FIELD-SYMBOL(<ls_levels>) INDEX ls_stat-level.

      IF ls_stat-colonrow IS INITIAL.

        "Getting tocken for the current statements.
        LOOP AT gr_scan->tokens INTO DATA(ls_token)
          FROM  ls_stat-from
          TO  ls_stat-to.
          APPEND ls_token TO gt_token.
          CONCATENATE lv_string ls_token-str INTO lv_string SEPARATED BY space.
          TRANSLATE lv_string TO UPPER CASE.
        ENDLOOP.
***

        CONDENSE lv_string.
        IF lv_string CP 'DATA+*+#MATNR+*'.
**--Begin of SJALLIPALL00
          READ TABLE gr_scan->tokens INTO ls_token INDEX ls_stat-from + 3.
          IF sy-subrc EQ 0 AND ls_token-str EQ 'MATNR'.
            CONTINUE.
          ENDIF.
**--End of SJALLIPALL00
          READ TABLE gr_scan->tokens INTO gs_token INDEX ls_stat-from.
          IF sy-subrc = 0.
            ls_remediate-row = gs_token-row.
          ENDIF.

        ENDIF.
        ls_remediate-level = ls_stat-level.                                             "SJALLIPALL00
        IF   ls_remediate-row IS NOT INITIAL.
          APPEND ls_remediate TO lt_remediate.
        ENDIF.
      ELSE.
**--Begin of SJALLIPALL00
**--chained statements
        "Check thet stmtn is DATA stmnt or not
        READ TABLE gr_scan->tokens INTO ls_token INDEX ls_stat-from .
        IF sy-subrc EQ 0  AND ls_token-str NP 'DATA*'.
          CONTINUE.
        ENDIF.
        LOOP AT lt_stmnt_tmp ASSIGNING FIELD-SYMBOL(<lfs_stmnt_tmp>) WHERE level = ls_stat-level
          AND  colonrow = ls_stat-colonrow.
          CLEAR : lv_string.
          READ TABLE gr_scan->tokens INTO ls_token INDEX <lfs_stmnt_tmp>-from + 1 .
          IF sy-subrc EQ 0 AND ls_token-str NP '*MATNR*'.
            CONTINUE.
          ENDIF.
          READ TABLE gr_scan->tokens INTO ls_token INDEX <lfs_stmnt_tmp>-from + 3 .
          IF sy-subrc EQ 0."  AND ls_token-str NP 'DATA*'.
            DATA(lv_token_idx) = sy-tabix.
            CASE ls_token-str.
              WHEN 'MATNR'.
                CONTINUE.
              WHEN 'RANGE'.
                "READ TABLE gr_scan->token INTO DATA(ls_token_tmp) INDEX lv_token_idx + 2
                CONTINUE.
            ENDCASE.
            APPEND  VALUE #(  level =  ls_stat-level row = <lfs_stmnt_tmp>-trow )  TO lt_remediate.
          ENDIF.
        ENDLOOP.
**--End of SJALLIPALL00
      ENDIF.


      CLEAR: ls_remediate,
           lv_string,
           gt_token.
    ENDLOOP.
    SORT  lt_remediate BY level row.
    DELETE ADJACENT DUPLICATES FROM lt_remediate COMPARING level row.
    DATA lv_insert TYPE i .
    " READ REPORT im_object INTO lt_src_code.        "SJALLIPALL00
    IF lt_remediate IS NOT INITIAL.
*      READ REPORT im_object INTO lt_src_code.
**--Begin of         "SJALLIPALL00
      " LOOP AT lt_remediate ASSIGNING FIELD-SYMBOL(<ls_remediate>).
      LOOP AT lt_remediate ASSIGNING FIELD-SYMBOL(<lfs_p_rem>)
             GROUP BY ( level =  <lfs_p_rem>-level )
             ASSIGNING FIELD-SYMBOL(<lfs_grp_rem>).
        READ TABLE gr_source_map WITH KEY level = <lfs_grp_rem>-level
        ASSIGNING FIELD-SYMBOL(<lfs_source>).
        IF sy-subrc EQ 0.
          lt_src_code = <lfs_source>-src_code.
        ENDIF.
        LOOP AT GROUP <lfs_grp_rem> ASSIGNING FIELD-SYMBOL(<ls_remediate>).
**--End of         "SJALLIPALL00
          READ TABLE lt_src_code ASSIGNING FIELD-SYMBOL(<ls_code_comment>) INDEX ( <ls_remediate>-row + lv_insert ).
*          DATA(lv_o_line_from) = sy-tabix.
*          DATA(lv_o_line_to) = sy-tabix.
*        READ TABLE lt_src_code INTO DATA(ls_code_comment_D) INDEX ( <ls_remediate>-row + lv_insert ).
          TRANSLATE <ls_code_comment> TO UPPER CASE.
*        IF <ls_code_comment> CP '"DATA+*+#MATNR+*'.
          IF <ls_code_comment> CP '*+#"+#DATA+*'.
            EXIT.
          ELSEIF  <ls_code_comment> CP 'DATA+*+#MATNR+*'.
            DATA(lv_length) = strlen( <ls_code_comment> ).
            IF <ls_code_comment> CP '*+:+*'.
              SPLIT <ls_code_comment> AT space INTO: DATA(str1) DATA(str2) DATA(str3) DATA(str4) DATA(str5) DATA(str6) DATA(str7), TABLE DATA(itab).
              CONDENSE str1.
              IF str1 CP '*DATA:*' OR str1 CP '*+#DATA:+*'."'DATA:+*'.
                SPLIT <ls_code_comment> AT space INTO: str1 str2 str3 str4 str5 str6, TABLE itab.
                DATA(lv_string_target) = str2.

              ELSE.
                SPLIT <ls_code_comment> AT space INTO: str1 str2 str3 str4 str5 str6, TABLE itab.
                lv_string_target = str3.
              ENDIF.

            ELSE.
              SPLIT <ls_code_comment> AT space INTO: str1 str3 str4 str5 str6 str7, TABLE itab.
              lv_string_target = str3.
            ENDIF.
            lv_insert = lv_insert + 1.

            INSERT gv_comment INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert ).
            APPEND <ls_code_comment> TO lt_o_src_code.
            CONCATENATE '"' <ls_code_comment> INTO <ls_code_comment>.

            lv_insert = lv_insert + 1.

            IF <ls_code_comment> CP '*.*' AND <ls_code_comment> CP '*:*'.
              DATA(lv_matnr_newcode) = | DATA: | && lv_string_target && | TYPE | && | MATNR. |.
            ELSEIF <ls_code_comment> CP '*,*' AND <ls_code_comment> CP '*:*'..
              lv_matnr_newcode = | DATA: | && lv_string_target && | TYPE | && | MATNR, |.
            ELSEIF <ls_code_comment> CP '*.*' AND <ls_code_comment> NP '*:*'.
              lv_matnr_newcode = | DATA | && lv_string_target && | TYPE | && | MATNR. |..
            ELSEIF <ls_code_comment> CP '*,*' AND <ls_code_comment> NP '*:*'..
              lv_matnr_newcode = | DATA | && lv_string_target && | TYPE | && | MATNR, |.
            ENDIF.
            lv_newcode = lv_matnr_newcode .
            INSERT lv_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert ).
*            APPEND lv_newcode TO lt_n_src_code.
*            DATA(lv_n_line_from) =   <ls_remediate>-row + lv_insert.
*            DATA(lv_n_line_to) =   <ls_remediate>-row + lv_insert.
          ELSE.
**--Begin of SJALLIPALL00
            " SPLIT <ls_code_comment> AT ':' INTO DATA(lv_data_key) DATA(lv_decl_str).
            " SPLIT lv_decl_str AT SPACE INTO : str1 str2 str3 str4 str5 str6 str7, TABLE itab.
            "     lv_string_target = str3.
            DATA(lv_string_tmp) = <ls_code_comment>.
            CONDENSE lv_string_tmp.
            SPLIT lv_string_tmp AT space INTO : str1 str2 str3 str4 str5 str6 str7, TABLE itab.
            lv_string_target = str1.
            IF <ls_code_comment> CP '*,*'." AND <ls_code_comment> CP '*:*'..
              lv_matnr_newcode =  lv_string_target && | TYPE | && | MATNR, |.
            ELSEIF <ls_code_comment> CP '*.*' AND <ls_code_comment> NP '*:*'.
              lv_matnr_newcode =  lv_string_target && | TYPE | && | MATNR. |..
            ENDIF.
            lv_insert = lv_insert + 1.
            INSERT gv_comment INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert ).
            APPEND <ls_code_comment> TO lt_o_src_code.
            CONCATENATE '"' <ls_code_comment> INTO <ls_code_comment>.
            lv_insert = lv_insert + 1.
            INSERT lv_matnr_newcode INTO lt_src_code INDEX ( <ls_remediate>-row + lv_insert ).
*            APPEND lv_newcode TO lt_n_src_code.
*            lv_n_line_from =   <ls_remediate>-row + lv_insert.
*            lv_n_line_to =   <ls_remediate>-row + lv_insert.
            "lv_matnr_newcode =
**--End of SJALLIPALL00
          ENDIF.
*          CALL METHOD me->meth_prep_log_msg
*            EXPORTING
*              im_remediation         = 'Replace MATNR Data element'
*              im_rem_obj_name        = im_object
*              im_old_code_start_line = CONV #( lv_o_line_from )
*              im_old_code_end_line   = CONV #( lv_o_line_to )
*              im_new_code_start_line = CONV #( lv_n_line_from )
*              im_new_code_end_line   = CONV #( lv_n_line_to )
*              im_t_old_src_code      = lt_o_src_code
*              im_t_new_src_code      = lt_n_src_code.
CLEAR : lv_newcode,lv_string_target.
        ENDLOOP.
        " ENDIF.                                                  "SJALLIPALL00

        "INSERT REPORT im_object FROM lt_src_code.                "SJALLIPALL00
         INSERT REPORT <lfs_source>-name FROM lt_src_code.         "SJALLIPALL00

** Syntax Check for the updated program
        CALL METHOD me->prog_syntax_check
          EXPORTING
            im_object  = <lfs_source>-name  "im_object "SJALLIPALL00
            im_srccode = lt_src_code
          IMPORTING
            et_warning = lt_warnings
            ex_line    = lv_line
            ex_word    = ls_word
            ex_msg     = ls_msg
            ex_dir     = ls_dir.
        IF lt_warnings IS NOT INITIAL.
          LOOP AT lt_warnings ASSIGNING FIELD-SYMBOL(<fs_warning>).
            APPEND INITIAL LINE TO ex_message ASSIGNING FIELD-SYMBOL(<lfs_message>).
            <lfs_message>-line = <fs_warning>-line.
            <lfs_message>-message = <fs_warning>-message.
            <lfs_message>-typ = 'E'.
          ENDLOOP.
          CLEAR : ev_insert.
        ELSE.
          ev_insert = abap_true.
          APPEND INITIAL LINE TO ex_message ASSIGNING <lfs_message>.
          <lfs_message>-typ = 'S'.
          <lfs_message>-message = TEXT-000.
          COMMIT WORK AND WAIT.
        ENDIF.
        CLEAR : lv_insert,lv_newcode.
      ENDLOOP.                                    "SJALLIPALL00
    ENDIF.                                        "SJALLIPALL00
  ENDMETHOD.


  METHOD set_counter.
    IF im_obj_name IS NOT INITIAL.
      IF gt_obj_cntr IS NOT INITIAL.

        DATA : lt_temp_tab  TYPE tt_obj_counter,
               lt_temp_tab2 TYPE tt_obj_counter.
        lt_temp_tab = gt_obj_cntr.
        lt_temp_tab2 = gt_obj_cntr.

        LOOP AT lt_temp_tab  ASSIGNING FIELD-SYMBOL(<lfs_counter>) WHERE sequence LT gv_seq.
          IF <lfs_counter>-at_line GE im_line_no.
            <lfs_counter>-at_line = <lfs_counter>-at_line + im_lines_add.
          ENDIF.
        ENDLOOP.

        CLEAR : gt_obj_cntr.
        gt_obj_cntr = lt_temp_tab.
      ENDIF.
      DATA: lwa_obj_cntr TYPE ty_obj_counter.

      lwa_obj_cntr-object_name = im_obj_name.
      lwa_obj_cntr-at_line = im_line_no.
      lwa_obj_cntr-insert = im_lines_add.
      lwa_obj_cntr-sequence = gv_seq.
      lwa_obj_cntr-row  = im_row.
      lwa_obj_cntr-comment  = im_comment_line.
      lwa_obj_cntr-org_line = im_org_line.

      INSERT lwa_obj_cntr INTO TABLE gt_obj_cntr.
      INSERT lwa_obj_cntr INTO TABLE gt_obj_cntr_temp.
    ENDIF.


  ENDMETHOD.


  METHOD meth_validate_src_code.
    CLEAR : ex_error.
    CALL METHOD me->prog_syntax_check
      EXPORTING
        im_object  = im_object
        im_srccode = im_t_src_code
      IMPORTING
        et_warning = DATA(lt_warnings)
        ex_line    = DATA(lv_line)
        ex_word    = DATA(ls_word)
        ex_msg     = DATA(ls_msg)
        ex_dir     = DATA(ls_dir).
    IF lt_warnings IS NOT INITIAL.
      LOOP AT lt_warnings ASSIGNING FIELD-SYMBOL(<fs_warning>).
        IF <fs_warning>-msgnumber = 'G46'.
          CONTINUE.
        ENDIF.
        APPEND INITIAL LINE TO gt_messages ASSIGNING FIELD-SYMBOL(<lfs_message>).
        <lfs_message>-line = <fs_warning>-line.
        <lfs_message>-message = <fs_warning>-message.
        <lfs_message>-typ = 'E'.
        ex_error = abap_true.
      ENDLOOP.

    ELSE.
      APPEND INITIAL LINE TO gt_messages ASSIGNING <lfs_message>.
      <lfs_message>-typ = 'S'."lc_success'.
      <lfs_message>-message = TEXT-000.
*        COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.


  METHOD get_counter.
    CLEAR : ex_counter.
    LOOP AT gt_obj_cntr INTO DATA(lwa_obj_cntr) WHERE object_name = im_obj_name AND
                                                       sequence NE gv_seq.
      IF lwa_obj_cntr-at_line > im_line_no.
        EXIT.
      ENDIF.
      ex_counter =  ex_counter + lwa_obj_cntr-insert.
      IF lwa_obj_cntr-at_line = im_line_no.
        ex_counter =  ex_counter + lwa_obj_cntr-comment.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD meth_get_delta_changes.
    "Code Scan
    CALL METHOD me->code_scan
      EXPORTING
        im_object     = im_object
      IMPORTING
        et_source_map = DATA(lt_new_src_code).
**--Fetch delta changes
    DATA: lt_o_code TYPE ABAPTXT255_tab,
          lt_n_code TYPE ABAPTXT255_tab,
          lt_delta  TYPE STANDARD TABLE OF xtrdir,
          trdir_sec TYPE STANDARD TABLE OF trdir,
          TRDIR_pri TYPE STANDARD TABLE OF trdir.
    DATA: abaptext_delta TYPE STANDARD TABLE OF vXABAPT255." OCCURS 0 WITH HEADER LINE..

    LOOP AT lt_new_src_code INTO DATA(lwa_new_src_code).
      READ TABLE gt_org_src_cod INTO DATA(lwa_old_src_code) WITH KEY level = lwa_new_src_code-level.
      IF sy-subrc EQ 0.
        "lt_o_code =  CORRESPONDING #( lwa_old_src_code-src_code mapping line = space ).
        "lt_n_code =  CORRESPONDING #( lwa_new_src_code-src_code ).
        lt_o_code = VALUE #(  FOR lwa_code IN lwa_old_src_code-src_code
        ( line = lwa_code ) ).

        lt_n_code = VALUE #(  FOR lwa_code IN lwa_new_src_code-src_code
        ( line = lwa_code ) ).

        CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
          EXPORTING
            compare_mode            = '1' "comp_mode'
            ignore_case_differences = 'X' "gv_ignore_case_differences
          TABLES
            texttab_old             = lt_n_code "lwa_old_src_code-src_code "abaptext_sec
            texttab_new             = lt_o_code "lwa_new_src_code-src_code "abaptext_pri
            trdirtab_old            = trdir_sec
            trdirtab_new            = trdir_pri
            trdir_delta             = lt_delta
            text_delta              = abaptext_delta.
        "ext_delta              = abaptext_delta.
        READ TABLE abaptext_delta INDEX 1 TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE."no_delta = true.
        ENDIF.
        DATA(l_start_line) =  0.
        DATA(l_end_line) = 0.
        DATA(l_index) = 0.
        APPEND VALUE #( msgty = 'I'    msgid = 'ZREMEDIATION' msgno = 000
        msgv1 = 'Object Name : '
        msgv2 = gv_object_name
        msgv3 = ' Remidate Object: '
        msgv4 = lwa_new_src_code-name ) TO gt_log_messages.
        LOOP AT abaptext_delta ASSIGNING FIELD-SYMBOL(<lfs_delta>).

          cl_message_helper=>set_msg_vars_for_clike( condense( <lfs_delta>-line ) ).

          DATA(lv_operation) = COND #( WHEN <lfs_delta>-vrsflag = 'U' THEN 'Update'
                                       WHEN <lfs_delta>-vrsflag = 'D' THEN 'Insert').


          APPEND VALUE #( msgty = 'I'    msgid = 'ZREMEDIATION' msgno = 000
                          msgv1 = 'Line no:' && space && <lfs_delta>-number && space && ' Operation:' && space && lv_operation && space
                          msgv2 = sy-msgv1
                          msgv3 = sy-msgv2 msgv4 = sy-msgv3 )
                        TO gt_log_messages.

        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD METH_CAPTURE_OBJ.

  DATA: lt_e071k        TYPE TABLE OF e071k,
lt_ko200        TYPE TABLE OF ko200.


  ENDMETHOD.


  METHOD meth_prep_log_msg.
    APPEND VALUE #( msgty = 'I'  msgid = 'ZREMEDIATION'  msgno = 000
                      msgv1 = 'Remediation: ' msgv2 =  im_remediation  ) TO gt_log_messages.
    APPEND VALUE #( msgty = 'I'  msgid = 'ZREMEDIATION'  msgno = 000
                      msgv1 = 'Remediate Object Name: ' msgv2 =  im_rem_obj_name  ) TO gt_log_messages.
    APPEND VALUE #( msgty = 'I'  msgid = 'ZREMEDIATION'  msgno = 000
                      msgv1 = |Old code | msgv2 = | From Line : { im_old_code_start_line } To Line : { im_old_code_end_line }|
                   ) TO gt_log_messages.


    LOOP AT im_t_old_src_code ASSIGNING FIELD-SYMBOL(<lfs_source_code>).
      cl_message_helper=>set_msg_vars_for_clike( condense( <lfs_source_code> ) ).
      APPEND VALUE #( msgty = 'I'    msgid = 'ZREMEDIATION' msgno = 000
      msgv1 = sy-msgv1 msgv2 = sy-msgv2
      msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) TO gt_log_messages.
    ENDLOOP.

    APPEND VALUE #( msgty = 'I'  msgid = 'ZREMEDIATION'   msgno = 000
    msgv1 = |New code| msgv2 = | From Line : { im_new_code_start_line } To Line : { im_new_code_end_line } | ) TO gt_log_messages.


    LOOP AT im_t_new_src_code ASSIGNING <lfs_source_code>.
      cl_message_helper=>set_msg_vars_for_clike( condense( <lfs_source_code> ) ).
      APPEND VALUE #( msgty = 'I'  msgid = 'ZREMEDIATION'   msgno = 000
      msgv1 = sy-msgv1 msgv2 = sy-msgv2
      msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) TO gt_log_messages.
    ENDLOOP.
  ENDMETHOD.


  METHOD meth_fetch_remediate_obj.
**----------------------------------------------------------------**
*Description : This method used to determine the object name that
*contains the source code of given repository object or group or
*class or FM and it's object type
*Created By  : SJALLIPALL00
**------------------------------------------------------------**
    CLEAR : ex_rem_obj,ex_error,ex_set_prety_print.

**--Check the source code repository table
    SELECT SINGLE * INTO  @DATA(ls_tadir) FROM tadir WHERE obj_name =  @im_object.
    IF sy-subrc EQ 0.
      ch_objtyp = ls_tadir-object.
    ELSE.
**--Check the function grop directory table
      SELECT SINGLE * INTO @DATA(ls_tfdir) FROM tfdir WHERE funcname = @im_object.
      IF sy-subrc EQ 0.
        ch_objtyp =  'FUNC'.
      ELSE.
        ex_error = abap_true.
      ENDIF.
    ENDIF.

    CASE ch_objtyp.
**--For CLASS fetch the class pool name
      WHEN 'CLAS'.
        DATA: lr_obj_name TYPE RANGE OF progname.
        lr_obj_name = VALUE #( ( sign = 'I' option = 'CP' low = im_object && '*' ) ).
        SELECT * UP TO 1 ROWS
           FROM trdir
           INTO @DATA(ls_trdir)
          WHERE name IN @lr_obj_name AND
          subc = 'K'
          ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc EQ 0.
          ex_rem_obj = ls_trdir-name.
        ELSE.
          ex_error = abap_true.
        ENDIF.
        ex_set_prety_print = abap_true.
**--Fetch methods
        SELECT classname , methodname FROM
          tmdir INTO TABLE @DATA(lt_methods)
          WHERE classname = @im_object.
        IF sy-subrc EQ 0.
          LOOP AT lt_methods ASSIGNING FIELD-SYMBOL(<lfs_methods>).

            DATA : ls_key    TYPE seocpdkey,
                   l_program TYPE program.
            ls_key-clsname = <lfs_methods>-classname.
            ls_key-cpdname = <lfs_methods>-methodname.

            CALL FUNCTION 'SEO_METHOD_GET_SOURCE'
              EXPORTING
                mtdkey                        = ls_key
              IMPORTING
                incname                       = l_program
              EXCEPTIONS
                _internal_method_not_existing = 1
                _internal_class_not_existing  = 2
                version_not_existing          = 3
                inactive_new                  = 4
                inactive_deleted              = 5
                OTHERS                        = 6.
            IF sy-subrc EQ 0.
* Implement suitable error handling here
              APPEND INITIAL LINE TO gt_object_details ASSIGNING FIELD-SYMBOL(<lfs_object_det>).
              <lfs_object_det>-src_object_name = ls_key-cpdname.
              <lfs_object_det>-sub_object_name = l_program.
            ENDIF.
          ENDLOOP.
        ENDIF.

      WHEN 'FUNC'.
**--For functio TFDRI-PNAME is the FG source include
        DATA: lv_fname        TYPE rs38l-name,
              lv_include_name TYPE rs38l-include.
        ex_rem_obj = ls_tfdir-pname.
        GV_is_func = abap_true.
        gv_fugr_name = ex_rem_obj.
      WHEN 'FUGR'.
**--For function group
        ex_rem_obj = |'SAPL' && ls_tadir-obj_name|.
        gv_is_fugr = abap_true.
      WHEN OTHERS.
        ex_rem_obj = im_object.
        ex_set_prety_print = abap_true.
    ENDCASE.
    "Code Scan
    CALL METHOD me->code_scan
      EXPORTING
        im_object     = ex_rem_obj
      IMPORTING
        et_source_map = gt_org_src_cod.

    gv_object_name = im_object.
    gv_obj_type = ch_objtyp.

    IF gv_is_func EQ abap_true or
       gv_is_fugr EQ abap_true.

      SPLIT ex_rem_obj AT 'SAPL' into data(l_namespace) data(l_fugr).
      data: lt_funtab type STANDARD table of SUNI_FUNCSTRUC.

      CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
       IMPORTING
         FUNCTAB                   = lt_funtab
       CHANGING
         GROUP                     = l_fugr
       EXCEPTIONS
         FUNCTION_NOT_EXISTS       = 1
         INCLUDE_NOT_EXISTS        = 2
         GROUP_NOT_EXISTS          = 3
         NO_SELECTIONS             = 4
         NO_FUNCTION_INCLUDE       = 5
         OTHERS                    = 6
                .
      IF sy-subrc EQ 0.
* Implement suitable error handling here
        gt_object_details = CORRESPONDING #( lt_funtab MAPPING src_object_name = funcname
                                                               sub_object_name = funcincl  ).
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD prog_syntax_check.
    DATA: dir  TYPE trdir.
    DATA:warnings_table       TYPE STANDARD TABLE OF rslinlmsg,
         correction_table     TYPE sedi_correction_table,
         l_trdir_source_entry TYPE trdir,
         l_trdir_rahmen_entry TYPE trdir,
         o_error_include      LIKE  sy-repid,
         o_message_id         TYPE  sedi_internal_id,
         o_error_descriptions TYPE  rslinltab,
         error_table          TYPE STANDARD TABLE OF  rslinlmsg.

    l_trdir_rahmen_entry-uccheck = 'X'.
    l_trdir_rahmen_entry-varcl = 'X'.
    l_trdir_rahmen_entry-varcl = 'X'.
    l_trdir_rahmen_entry-rmand = '100'.
    l_trdir_rahmen_entry-fixpt = 'X'.
    l_trdir_rahmen_entry-subc = '1'.
    SYNTAX-CHECK FOR im_srccode
           MESSAGE ex_msg
           LINE    ex_line
           WORD    ex_word
           PROGRAM im_object
           FRAME ENTRY l_trdir_rahmen_entry
           INCLUDE o_error_include
           MESSAGE-ID o_message_id
           ID 'ERR'  TABLE et_warning.


  ENDMETHOD.


  METHOD read_remediation.

    CONSTANTS : LC_read        TYPE string VALUE 'READ',
                lc_key         TYPE string VALUE 'KEY',
                lc_binary_srch TYPE string VALUE '*BINARY SEARCH.',
                lc_binary      TYPE string VALUE 'BINARY SEARCH.',
                lc_index       TYPE string VALUE '*INDEX*.',
                lc_sort        TYPE string VALUE 'SORT',
                lc_by          TYPE STring VALUE 'BY',
                lc_eq          TYPE string VALUE '=',
                lc_info        TYPE char1 VALUE 'I',
                lc_error       TYPE char1 VALUE 'E',
                lc_success     TYPE char1 VALUE 'S'.

    DATA : lr_scan            TYPE REF TO cl_ci_scan,
           lv_line            TYPE i,
           lv_counter         TYPE i,
           ls_word            TYPE string,
           lv_pgm_name        TYPE string,
           ls_dir             TYPE trdir,
           ls_msg             TYPE string,
           lt_warnings        TYPE synt_errors,
           lv_fld             TYPE string,
           lt_source_map      TYPE tt_source_map,
           lt_source_code     TYPE string_table,
           lt_old_source_code TYPE string_table,
           lt_new_src_code    TYPE string_table,
           lv_curr_cntr       TYPE i,
           lt_remediation_obj TYPE SORTED TABLE OF zstr_code_remediation
                                   WITH NON-UNIQUE KEY remediation_type.
    DATA: lt_lines TYPE STANDARD TABLE OF i.

    gv_seq += 1.
    IF gv_apply_changes IS INITIAL.
      "Code Scan
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = im_object
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = DATA(lt_source_map_temp).

    ELSE.
      lt_source_map_temp = VALUE #( FOR lwa_remed IN gt_remediation WHERE  ( remediation_type = 'READ REMEDIATION' )
                                                    ( name = lwa_remed-obj_name )  ).
      SORT lt_source_map_temp BY name.
      DELETE ADJACENT DUPLICATES FROM lt_source_map_temp COMPARING name.
    ENDIF.

    LOOP AT lt_source_map_temp ASSIGNING FIELD-SYMBOL(<lfs_smap>).

      CALL METHOD me->code_scan
        EXPORTING
          im_object     = <lfs_smap>-name
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = lt_source_map.
**-->Tokens will store all the keywords including nested includes too.
      "Since it will have all the tokens copy the current level tokens only.
      DATA(lv_level) = lt_source_map[ name = <lfs_smap>-name ]-level.
      DATA(lt_temp_stmnts) = lr_scan->statements.

      SORT lt_temp_stmnts BY level from.
      DATA(lv_token_from) = lt_temp_stmnts[ level = lv_level ]-from.

      SORT lt_temp_stmnts BY level ASCENDING from DESCENDING.
      DATA(lv_token_to) =  lt_temp_stmnts[ level = lv_level ]-to.


      IF gv_apply_changes EQ abap_true.
        CLEAR lt_lines.
        lt_lines = VALUE #( FOR lwa_remed IN gt_remediation WHERE  (
        obj_name = <lfs_smap>-name AND
        remediation_type = 'READ REMEDIATION'    )
           ( lwa_remed-line_no )  ).
      ENDIF.
      "Fetch statements with READ query
      LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_scan>) WHERE str = lc_read.
        DATA(lv_index) = sy-tabix.
        IF lv_index > lv_token_to.
          EXIT.
        ENDIF.
        DATA(lv_row_read) = <ls_scan>-row.

        READ TABLE lr_scan->tokens INTO DATA(ls_token) INDEX lv_index + 1.
        IF sy-subrc = 0 AND ls_token-str = 'TABLE'.
********* Read key fields from READ statement.
          READ TABLE lr_scan->tokens INTO DATA(ls_tok_scan) INDEX lv_index + 2.
          IF sy-subrc EQ 0.
            DATA(lv_sort) = | { lc_sort } { ls_tok_scan-str } { lc_by } |.
          ENDIF.
          READ TABLE lr_scan->statements ASSIGNING FIELD-SYMBOL(<lfs_keys>)
                                                  WITH KEY from = lv_index.
          IF sy-subrc EQ 0.
            "Get the fields from REAd statement to use in SORT
            READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_from>) INDEX <lfs_keys>-from.
            IF sy-subrc EQ 0.
              DATA(lv_src_line_from) = <ls_token_from>-row.
            ENDIF.
            READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_to>) INDEX <lfs_keys>-to.
            IF sy-subrc EQ 0.
              DATA(lv_src_line_to) = <ls_token_to>-row.
            ENDIF.
            IF gv_apply_changes EQ abap_true.
              CALL METHOD me->get_counter
                EXPORTING
                  im_obj_name = <lfs_smap>-name
                  im_line_no  = lv_src_line_from
                IMPORTING
                  ex_counter  = DATA(lv_g_cntr).
              "lv_g_cntr = lv_g_cntr - lv_counter.
              READ TABLE lt_lines INTO lv_line WITH  KEY  table_line = lv_src_line_from - lv_g_cntr."<lfs_glb_cntr>-counter.
              IF sy-subrc NE 0.
                CONTINUE.
              ENDIF.
            ENDIF.
            LOOP AT lr_scan->tokens INTO ls_tok_scan FROM lv_index WHERE str = lc_key.
              DATA(lv_key_ind) = sy-tabix + 1.

              WHILE lv_key_ind <= <lfs_keys>-to.
                READ TABLE lr_scan->tokens INTO DATA(ls_tokens) INDEX lv_key_ind.
                IF sy-subrc EQ 0.
                  DATA(lv_idx) = lv_key_ind.
                  lv_key_ind = lv_key_ind + 3.
                  READ TABLE lr_scan->tokens INTO DATA(ls_tokens_temp) INDEX lv_idx + 1.
                  IF sy-subrc EQ 0 AND ls_tokens_temp-str EQ lc_eq.
                    lv_fld = |{ lv_fld } { ls_tokens-str }|.
                  ENDIF.
                ENDIF.
              ENDWHILE.
              EXIT.
            ENDLOOP.
          ENDIF.

          CONDENSE lv_fld.
          "Generate SORT statement
          lv_sort = |{ lv_sort }{ lv_fld } .|.
          CONDENSE lv_sort.
          "Get end line of one statement
          LOOP AT lr_scan->statements ASSIGNING FIELD-SYMBOL(<ls_statements>)
                                                WHERE from <= lv_index AND
                                                      to   >= lv_index.
            lv_level = <ls_statements>-level.
            DATA(lv_row) = <ls_statements>-trow.
            EXIT.
          ENDLOOP.

          "Place binary search in read statement
          READ TABLE lt_source_map ASSIGNING FIELD-SYMBOL(<ls_source_map>) WITH KEY level = lv_level.
          IF sy-subrc = 0 .
            IF lv_pgm_name <> <ls_source_map>-name.
              DATA(lt_source) = <ls_source_map>-src_code.
              CLEAR lv_counter.
            ENDIF.
            DATA(lt_temp_source) = lt_source.
            lv_pgm_name = <ls_source_map>-name.

            READ TABLE <ls_source_map>-src_code ASSIGNING FIELD-SYMBOL(<ls_src_code>) INDEX lv_row.
            IF sy-subrc = 0.
              IF <ls_src_code> CP lc_index.
              ELSEIF  <ls_src_code> CP lc_binary_srch
                OR <ls_src_code> CP '*SEARCH*'.
*Case where Binary Search exists
*Put Sort statement before read with binary search.

*Check whether Sorting is Done Already
                DATA(lv_temp_cntr) = lv_row_read.
                DATA: lv_temp_code TYPE string.
                DO 3 TIMES.
                  READ TABLE <ls_source_map>-src_code
                          ASSIGNING <ls_src_code> INDEX lv_temp_cntr.

                  lv_temp_code = |{ <ls_src_code> } { lv_temp_code }|.
                  lv_temp_cntr = lv_temp_cntr - 1.
                ENDDO.

                CONDENSE lv_temp_code.
                TRANSLATE lv_temp_code TO UPPER CASE.
                TRANSLATE lv_sort TO UPPER CASE.

                IF lv_temp_code CS lv_sort .
                ELSE.

                  INSERT |{ lv_sort } { gv_comment }|
                         INTO lt_source INDEX lv_counter + lv_row_read.

                  lv_curr_cntr += 1.
                  CALL METHOD me->set_counter
                    EXPORTING
                      im_obj_name  = <lfs_smap>-name
                      im_line_no   = lv_row_read + lv_counter
                      im_lines_add = lv_curr_cntr
                      im_row       = lv_row_read
                      im_org_line  = lv_line.
                  lv_counter = lv_counter + 1.
                  ev_insert = abap_true.

                ENDIF.
                CLEAR: lv_temp_cntr, lv_temp_code.
              ELSE.
*Case where Binary Search does not exist
                DATA(len) = strlen( <ls_src_code> ) - 1 .
                DATA(ls_temp) = |{ <ls_src_code>(len) } { lc_binary }|.
                MODIFY lt_source FROM ls_temp INDEX lv_row + lv_counter.
*              lv_newline_from =  lv_counter + lv_row_read.
                APPEND ls_temp TO  lt_new_src_code.
                CLEAR ls_temp.
*Check whether Sorting is Done Already
                lv_temp_cntr = lv_row_read.
                DO 3 TIMES.
                  READ TABLE <ls_source_map>-src_code
                          ASSIGNING <ls_src_code> INDEX lv_temp_cntr.

                  lv_temp_code = |{ <ls_src_code> } { lv_temp_code }|.
                  lv_temp_cntr = lv_temp_cntr - 1.
                ENDDO.

                CONDENSE lv_temp_code.
                TRANSLATE lv_temp_code TO UPPER CASE.
                TRANSLATE lv_sort TO UPPER CASE.


                FIND ALL OCCURRENCES OF |* { lv_sort } * |
                  IN TABLE <ls_source_map>-src_code FROM 1 TO lv_row_read
                  RESULTS DATA(results).

                IF lv_temp_code CS lv_sort .
                ELSE.

                  INSERT |{ lv_sort } { gv_comment }|
                         INTO lt_source INDEX lv_counter + lv_row_read.

                  lv_curr_cntr += 1.
                  CALL METHOD me->set_counter
                    EXPORTING
                      im_obj_name  = <lfs_smap>-name
                      im_line_no   = lv_row_read + lv_counter
                      im_lines_add = lv_curr_cntr
                      im_row       = lv_row_read
                      im_org_line  = lv_line.
                  lv_counter = lv_counter + 1.
                  ev_insert = abap_true.

*                lv_newline_to = lv_counter + lv_row_read + lv_org_lines.
                  APPEND |{ lv_sort } | TO lt_new_src_code.
                ENDIF.
                CLEAR: lv_temp_cntr, lv_temp_code.
                ev_insert = abap_true.

              ENDIF.
              IF ev_insert EQ abap_true.
**--Copy the Old/existing  Code to add in application log
**--Begin of SJALLIPALL00
                IF   gv_apply_changes EQ abap_false. "SAI V2.0
                  DATA: lv_o_src TYPE string,
                        lv_n_src TYPE string.
                  LOOP AT <ls_source_map>-src_code ASSIGNING <ls_src_code>
                    FROM lv_src_line_from TO lv_src_line_to.
                    CONCATENATE lv_o_src <ls_src_code> INTO lv_o_src SEPARATED BY space.
                    APPEND <ls_src_code> TO lt_old_source_code.
                  ENDLOOP.

**--BEgin of V2.0
                  APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<lfs_result>).
                  <lfs_result>-obj_name =   <ls_source_map>-name.
                  <lfs_result>-remediation_type = 'READ REMEDIATION'.
                  <lfs_result>-before = lv_o_src.
                  <lfs_result>-line_no = lv_src_line_from.
**--End of V2.0

                  DATA(lv_org_lines) = lines( lt_old_source_code ).
                  lv_temp_cntr = lv_counter - 1.
                  LOOP AT lt_source ASSIGNING <ls_src_code>
                    FROM lv_src_line_from + lv_temp_cntr TO lv_src_line_to + lv_counter.
                    CONCATENATE lv_n_src <ls_src_code> INTO lv_n_src SEPARATED BY space.
                  ENDLOOP.
                  <lfs_result>-after = lv_n_src.
                ENDIF.
**--Validate the modified source code if thre are no errors INSERTthe code.
                CALL METHOD me->meth_validate_src_code
                  EXPORTING
                    im_object     = <ls_source_map>-name
                    im_t_src_code = lt_source
                  IMPORTING
                    ex_error      = DATA(lv_error).
                IF lv_error EQ abap_false." AND
                  IF   gv_apply_changes EQ abap_true. "SAI V2.0
                    INSERT REPORT <ls_source_map>-name FROM
                    lt_source.
                  ENDIF.      "SAI V2.0
                ELSE.
                  lt_source = lt_temp_source.
                  lv_counter =  lv_counter - lv_curr_cntr.
                ENDIF.
**--End of SJALLIPALL00
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.


**--Clear the variables
        CLEAR : lv_sort,lv_src_line_to,
                lv_fld,"lv_newline_from,
                lv_key_ind,"lv_newline_to,
                ls_tok_scan,lt_old_source_code,
                ls_tokens_temp,lt_new_src_code,
                ls_msg,lv_curr_cntr,
                lv_line,lv_src_line_from,
                lv_o_src,lv_n_src,
                ev_insert.
      ENDLOOP.
      CLEAR lv_counter.
    ENDLOOP.
    me->adjust_counter( ).
  ENDMETHOD.


  METHOD read_write_on_db_remediation.

** Structure
    TYPES: BEGIN OF ty_operation,
             operation TYPE char10,
           END OF ty_operation.
** Constants
    CONSTANTS: "lc_upd_comment TYPE string VALUE '  " Updated through remediation tool',
      lc_select TYPE string VALUE 'SELECT',
      lc_delete TYPE string VALUE 'DELETE',
      lc_insert TYPE string VALUE 'INSERT',
      lc_update TYPE string VALUE 'UPDATE',
      lc_modify TYPE string VALUE 'MODIFY'.
** Data
    DATA: lt_operation   TYPE STANDARD TABLE OF ty_operation,
          itab           TYPE STANDARD TABLE OF string,
          lr_scan        TYPE REF TO cl_ci_scan,
          lv_line        TYPE i,
          ls_word        TYPE string,
          ls_dir         TYPE trdir,
          ls_msg         TYPE string,
          lt_warnings    TYPE synt_errors,
          lv_fld         TYPE string,
          lt_source_map  TYPE tt_source_map,
          ls_obj_details TYPE ty_obj_details,
          lin            TYPE i,
          wrd            TYPE string,
          dir            TYPE trdir,
          msg            TYPE string,
          uc             TYPE trdir-uccheck,
          lv_counter     TYPE i,
          lv_string      TYPE string,
          lv_string1     TYPE string,
          lv_string2     TYPE string.
    DATA: lt_token TYPE stokesx_tab.

**---Begin of V2.0
    DATA : lv_n_src     TYPE string,
           lv_o_src     TYPE string,
           lt_src_code  TYPE TABLE OF string,
           lv_program   TYPE string,
           lv_temp_cntr TYPE i,
           lv_curr_cntr TYPE i.

** Clearing the exporting parameters.
    CLEAR: ev_insert,ex_message.

** Filling the operation table.
    APPEND INITIAL LINE TO lt_operation ASSIGNING FIELD-SYMBOL(<lfs_operation>).
    <lfs_operation>-operation = lc_insert.
    APPEND INITIAL LINE TO lt_operation ASSIGNING <lfs_operation>.
    <lfs_operation>-operation = lc_delete.
    APPEND INITIAL LINE TO lt_operation ASSIGNING <lfs_operation>.
    <lfs_operation>-operation = lc_modify.
    APPEND INITIAL LINE TO lt_operation ASSIGNING <lfs_operation>.
    <lfs_operation>-operation = lc_update.
    APPEND INITIAL LINE TO lt_operation ASSIGNING <lfs_operation>.
    <lfs_operation>-operation = lc_select.
    gv_seq += 1.
** Get the view/table name against the old table from the mapping DB table.
    SELECT tabname,
            view_write,
            view_read
       FROM zdt_tab_vs_view
       INTO TABLE @DATA(lt_tabname)
       WHERE tabname NE @space.
**--Fetch the updated token details for every operation


    DATA:      lt_remediation_obj TYPE SORTED TABLE OF zstr_code_remediation
                                  WITH NON-UNIQUE KEY remediation_type.
    DATA: lt_lines TYPE STANDARD TABLE OF i.
    IF gv_apply_changes IS INITIAL.
      "Code Scan
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = im_object
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = DATA(lt_source_map_temp).

    ELSE.
      lt_source_map_temp = VALUE #( FOR lwa_remed IN gt_remediation WHERE  ( remediation_type = 'READ_WRITE_ON_DB_REMEDIATION' )
                                                    ( name = lwa_remed-obj_name )  ).
      SORT lt_source_map_temp BY name.
      DELETE ADJACENT DUPLICATES FROM lt_source_map_temp COMPARING name.
    ENDIF.


** Read the program code
** FInd the insert/update/modify etc and check the table name

    LOOP AT lt_source_map_temp ASSIGNING FIELD-SYMBOL(<lfs_smap>).
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = <lfs_smap>-name
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = lt_source_map.
      LOOP AT lt_operation ASSIGNING <lfs_operation>.

        DATA(lv_op_index) = 1.
        IF gv_apply_changes EQ abap_true.
          CLEAR lt_lines.
          lt_lines = VALUE #( FOR lwa_remed IN gt_remediation WHERE  (
                                obj_name = <lfs_smap>-name AND
                                remediation_type = 'READ_WRITE_ON_DB_REMEDIATION'   )
                                   ( lwa_remed-line_no )  ).
        ENDIF.
        WHILE lv_op_index IS NOT INITIAL.
          LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_scan>) FROM lv_op_index WHERE str = <lfs_operation>-operation.
            DATA(lv_indx) = sy-tabix.
            LOOP AT lr_scan->statements ASSIGNING FIELD-SYMBOL(<lfs_keys>) WHERE from <= lv_indx AND
                                                           to   >= lv_indx.
            ENDLOOP.
            DATA(lv_from) = <lfs_keys>-from.
            DATA(lv_to) = <lfs_keys>-to.

**--Fetchin starting and ending line numbers of current statement
            READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_from>) INDEX lv_from.
            IF sy-subrc EQ 0.
              DATA(lv_src_line_from) = <ls_token_from>-row.

            ENDIF.
            READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_to>) INDEX lv_to.
            IF sy-subrc EQ 0.
              DATA(lv_src_line_to) = <ls_token_to>-row.
            ENDIF.

**Set Index for next itearation
            lv_op_index = <lfs_keys>-to + 1.
            IF gv_apply_changes EQ abap_true.

              CALL METHOD me->get_counter
                EXPORTING
                  im_obj_name = <lfs_smap>-name
                  im_line_no  = lv_src_line_from
                IMPORTING
                  ex_counter  = DATA(lv_g_cntr).
              "lv_g_cntr = lv_g_cntr - lv_counter.
              READ TABLE lt_lines INTO lv_line WITH
                        KEY  table_line = lv_src_line_from - lv_g_cntr."<lfs_glb_cntr>-counter.
              IF sy-subrc NE 0.
                CONTINUE.
              ENDIF.

            ENDIF.

**--Set operation index for next iteration

            CLEAR lt_token.
**--getting token for the current statements.
            LOOP AT lr_scan->tokens INTO DATA(ls_token)
              FROM  lv_from
              TO  lv_to.
              APPEND ls_token TO lt_token.
            ENDLOOP.
**--Find the source teable name which need replacement
            LOOP AT lt_tabname ASSIGNING FIELD-SYMBOL(<lfs_tabname>).
              LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_scan1>) FROM lv_from  TO lv_to WHERE str = <lfs_tabname>-tabname.
                DATA(lv_index) = sy-tabix.
                DATA(lv_row) = <ls_scan1>-row.
                LOOP AT lr_scan->statements ASSIGNING <lfs_keys>
                                                         WHERE from <= lv_index AND
                                                               to   >= lv_index.
                ENDLOOP.
                IF <lfs_keys> IS ASSIGNED.
                  READ TABLE lt_source_map ASSIGNING FIELD-SYMBOL(<ls_source_map>) WITH KEY level = <lfs_keys>-level.
                  IF sy-subrc IS INITIAL.
**--Begin of V2.0
                    IF lv_program <> <ls_source_map>-name.
                      lv_program = <ls_source_map>-name.
                      lt_src_code =  <ls_source_map>-src_code.
                      CLEAR : lv_counter.
                    ENDIF.
                    DATA(lt_temp_src_code) = lt_src_code.
                    DATA(lv_src_n_from) =  lv_src_line_from + lv_counter.
                    DATA(lv_src_n_to)   =  lv_src_line_to + lv_counter.
**--End of V2.0
                    READ TABLE  lt_src_code"<ls_source_map>-src_code   V2.0
                      ASSIGNING FIELD-SYMBOL(<ls_src_code>) INDEX lv_row + lv_counter.
                    IF sy-subrc IS INITIAL.
                      lv_string = <ls_src_code>.
                      DATA(lv_old_string) = <ls_src_code>.
                      TRANSLATE lv_string TO UPPER CASE.
                      FIND <lfs_tabname>-tabname IN  lv_string.
                      IF sy-subrc IS INITIAL.
** Only for reading the data
                        IF <lfs_operation>-operation = lc_select.
                          TRANSLATE <lfs_tabname>-view_read TO LOWER CASE.
                          REPLACE <lfs_tabname>-tabname IN lv_string  WITH <lfs_tabname>-view_read.
                        ELSE.
** Only for writing to DB
                          TRANSLATE <lfs_tabname>-view_write TO LOWER CASE.
                          REPLACE <lfs_tabname>-tabname IN lv_string  WITH <lfs_tabname>-view_write.
                        ENDIF.
                        <ls_src_code>   = lv_string.
                        <ls_src_code>   = |{ <ls_src_code> } { gv_comment }|.
                        DATA(lv_insert) = abap_true.
                        DATA(lv_ins_tab) = abap_true.

** Comment the old code
                        CONCATENATE '"' lv_old_string INTO lv_old_string.
                        INSERT lv_old_string INTO lt_src_code "<ls_source_map>-src_code  "V2.0
                            INDEX lv_row + lv_counter.
                        "lv_src_line_to += 1.
                        CALL METHOD me->set_counter
                          EXPORTING
                            im_obj_name  = <lfs_smap>-name
                            im_line_no   = lv_row + lv_counter + 1
                            im_lines_add = 1
                            im_org_line  = lv_line
                            im_row       = lv_row.
                        lv_src_n_to += 1.
                        lv_counter += 1. "V2.0
                        lv_temp_cntr += 1.

                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
                IF lv_ins_tab IS NOT INITIAL.
                  IF <lfs_operation>-operation = lc_select.
                    LOOP AT lt_src_code "<ls_source_map>-src_code
                       ASSIGNING <ls_src_code> FROM  lv_src_n_from "lv_src_line_from
                                                TO   lv_src_n_to.  "lv_src_line_to.
                      CLEAR: lv_index,
                             lv_row,
                             lv_string,
                             lv_string2,
                             lv_string1,
                             lv_old_string.

                      lv_row = sy-tabix.
                      lv_string = <ls_src_code>.
                      lv_old_string = <ls_src_code>.

                      TRANSLATE lv_string TO UPPER CASE.
** Only for reading the data
                      IF ( lv_string CP '*+#INTO+*' OR
                           lv_string CP '*INTO+*').
                        IF lv_string NP '*+#@DATA+*'.
                          IF lv_string NP '*+#CORRESPONDING FIELDS OF+*'.
                            SPLIT lv_string AT 'INTO' INTO lv_string1 lv_string2.
                            CLEAR lv_string.
                            CONCATENATE lv_string1 'INTO CORRESPONDING FIELDS OF' lv_string2 INTO lv_string SEPARATED BY space.
                            <ls_src_code>   = lv_string.
                            <ls_src_code> = |{ <ls_src_code> } { gv_comment }|.
** Comment the old code
                            CONCATENATE '"' lv_old_string INTO lv_old_string.
                            INSERT lv_old_string INTO lt_src_code"<ls_source_map>-src_code
                              INDEX lv_row ."+ lv_counter.
                            CALL METHOD me->set_counter
                              EXPORTING
                                im_obj_name  = <lfs_smap>-name
                                im_line_no   = lv_row
                                im_lines_add = 1.
                            lv_counter += 1. "V2.0
                            lv_temp_cntr += 1.

                            EXIT.
                          ELSE.
                            EXIT.
                          ENDIF.
                        ELSE.
                          EXIT.
                        ENDIF.
                      ENDIF.
                    ENDLOOP.
                  ENDIF.
*                ENDIF.
** Update the line with new code.
*                INSERT REPORT <ls_source_map>-name FROM <ls_source_map>-src_code. V2.0
                  EXIT.
                ENDIF.
              ENDLOOP.
              CLEAR: lv_row,lv_index,lv_string,lv_string1,lv_string2,
                     lv_old_string.
              IF lv_ins_tab IS NOT INITIAL.
                EXIT.
              ENDIF.
            ENDLOOP.
            IF lv_ins_tab IS NOT INITIAL.
              EXIT.
            ENDIF.
            CLEAR : lv_src_line_from,lv_src_line_to,lv_src_n_to,lv_src_n_from.
          ENDLOOP.
          IF sy-subrc NE 0.
            CLEAR lv_op_index.
            EXIT.
          ENDIF.

          CLEAR: lv_indx  .
          IF lv_ins_tab IS NOT INITIAL.
**--Begin of SJALLIPALL00
            LOOP AT <ls_source_map>-src_code ASSIGNING <ls_src_code>
              FROM lv_src_line_from TO lv_src_line_to.
              CONCATENATE lv_o_src <ls_src_code> INTO lv_o_src SEPARATED BY space.
*            APPEND <ls_src_code> TO lt_old_source_code.
            ENDLOOP.

**--BEgin of V2.0
            APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<lfs_result>).
            <lfs_result>-obj_name =   <ls_source_map>-name.
            <lfs_result>-remediation_type = 'READ_WRITE_ON_DB_REMEDIATION'.
            <lfs_result>-before = lv_o_src.
            <lfs_result>-line_no = lv_src_line_from.
**--End of V2.0
            LOOP AT lt_src_code ASSIGNING <ls_src_code>
              FROM lv_src_n_from TO lv_src_n_to.
              CONCATENATE lv_n_src <ls_src_code> INTO lv_n_src SEPARATED BY space.
            ENDLOOP.
            <lfs_result>-after = lv_n_src.
**--Validate the modified source code if thre are no errors INSERTthe code.
            CALL METHOD me->meth_validate_src_code
              EXPORTING
                im_object     = <ls_source_map>-name
                im_t_src_code = lt_src_code
              IMPORTING
                ex_error      = DATA(lv_error).
            IF lv_error EQ abap_false AND  gv_apply_changes EQ abap_true. "SAI V2.0
              INSERT REPORT <ls_source_map>-name FROM
              lt_src_code.
            ELSE.
              lv_counter = lv_counter - lv_temp_cntr.
              lt_src_code = lt_temp_src_code.
            ENDIF.      "SAI V2.0
**--End of SJALLIPALL00
            CLEAR : lv_ins_tab,lv_src_n_from,lv_src_n_to,lv_n_src,lv_o_src,lv_src_line_from,lv_src_line_to,lv_temp_cntr.
          ENDIF.
        ENDWHILE.
        CLEAR :
      lt_warnings,lv_row,lv_index,lv_string,ls_word,lv_temp_cntr.
      ENDLOOP.
    ENDLOOP.
    ev_insert = lv_insert.

    CLEAR: lv_insert,ls_dir,ls_msg,lv_line,lt_source_map,lt_source_map,
    lt_warnings,lv_row,lv_index,lv_string,lt_tabname,ls_word,lv_temp_cntr.
  ENDMETHOD.


  METHOD delete_duplicate.
    CONSTANTS : lc_delete      TYPE string VALUE 'DELETE',
                lc_comparing   TYPE string VALUE 'COMPARING',
                lc_binary_srch TYPE string VALUE '*BINARY SEARCH.',
                lc_binary      TYPE string VALUE '.',
                lc_index       TYPE string VALUE '*INDEX*.',
                lc_sort        TYPE string VALUE 'SORT',
                lc_by          TYPE STring VALUE 'BY',
                lc_eq          TYPE string VALUE '=',
                lc_info        TYPE char1 VALUE 'I',
                lc_error       TYPE char1 VALUE 'E',
                lc_success     TYPE char1 VALUE 'S'.

    DATA : lr_scan        TYPE REF TO cl_ci_scan,
*           lv_object     TYPE  e071-obj_name,
           lv_line        TYPE i,
           lv_counter     TYPE i,
           ls_word        TYPE string,
           lv_pgm_name    TYPE string,
           ls_dir         TYPE trdir,
           ls_msg         TYPE string,
           lt_warnings    TYPE synt_errors,
           lv_fld         TYPE string,
           lt_source_map  TYPE tt_source_map,
           lt_source_code TYPE string_table,
           lv_o_src       TYPE string,
           lv_n_src       TYPE String,
           lv_curr_cntr   TYPE i.

    "Code Scan
    DATA: lt_lines TYPE STANDARD TABLE OF i.
    gv_seq += 1.
    IF gv_apply_changes IS INITIAL.
      "Code Scan
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = im_object
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = DATA(lt_source_map_temp).

    ELSE.
      lt_source_map_temp = VALUE #( FOR lwa_remed IN gt_remediation WHERE  ( remediation_type = 'DELETE DUPLICATE' )
                                                    ( name = lwa_remed-obj_name )  ).
      SORT lt_source_map_temp BY name.
      DELETE ADJACENT DUPLICATES FROM lt_source_map_temp COMPARING name.

    ENDIF.

    LOOP AT lt_source_map_temp ASSIGNING FIELD-SYMBOL(<lfs_map>).
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = <lfs_map>-name
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = lt_source_map.
      "Fetch statements with DELETE query
**-->Tokens will store all the keywords including nested includes too.
      "Since it will have all the tokens copy the current level tokens only.
      DATA(lv_level) = lt_source_map[ name = <lfs_map>-name ]-level.
      DATA(lt_temp_stmnts) = lr_scan->statements.

      SORT lt_temp_stmnts BY level from.
      DATA(lv_token_from) = lt_temp_stmnts[ level = lv_level ]-from.

      SORT lt_temp_stmnts BY level ASCENDING from DESCENDING.
      DATA(lv_token_to) =  lt_temp_stmnts[ level = lv_level ]-to.

      IF gv_apply_changes EQ abap_true.
        CLEAR lt_lines.
        lt_lines = VALUE #( FOR lwa_remed IN gt_remediation WHERE  (
        obj_name = <lfs_map>-name AND
        remediation_type = 'DELETE DUPLICATE'    )
           ( lwa_remed-line_no )  ).
      ENDIF.

      LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_scan>) WHERE str = lc_delete. "commented by sagar
        DATA(lv_index) = sy-tabix.

        IF lv_index > lv_token_to.
          EXIT.
        ENDIF.

        DATA(lv_row_read) = <ls_scan>-row.
        DATA(lv_row_read_1) = <ls_scan>-row.

        READ TABLE lr_scan->tokens INTO DATA(ls_token) INDEX lv_index + 1.
        IF sy-subrc = 0 AND ls_token-str = 'ADJACENT'.
          READ TABLE lr_scan->tokens INTO DATA(ls_tokena) INDEX lv_index + 2.
          IF sy-subrc = 0 AND ls_tokena-str = 'DUPLICATES'.
********* Read int_tab name from DELETE statement.
            READ TABLE lr_scan->tokens INTO DATA(ls_tok_scan) INDEX lv_index + 4.
            IF sy-subrc EQ 0.
              DATA(lv_sort) = | { lc_sort } { ls_tok_scan-str } { lc_by } |.
            ENDIF.
            READ TABLE lr_scan->statements ASSIGNING FIELD-SYMBOL(<lfs_keys>)
                                                    WITH KEY from = lv_index.
            IF sy-subrc EQ 0.
              "Get the fields from DELETE statement to use in SORT
              READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_from>) INDEX <lfs_keys>-from.
              IF sy-subrc EQ 0.
                DATA(lv_src_line_from) = <ls_token_from>-row.
              ENDIF.
              READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_to>) INDEX <lfs_keys>-to.
              IF sy-subrc EQ 0.
                DATA(lv_src_line_to) = <ls_token_to>-row.
              ENDIF.
              IF gv_apply_changes EQ abap_true.
                CALL METHOD me->get_counter
                  EXPORTING
                    im_obj_name = <lfs_map>-name
                    im_line_no  = lv_src_line_from
                  IMPORTING
                    ex_counter  = DATA(lv_g_cntr).
                "lv_g_cntr = lv_g_cntr - lv_counter.
                READ TABLE lt_lines INTO lv_line

                 WITH  KEY  table_line = lv_src_line_from - lv_g_cntr."<lfs_glb_cntr>-counter.
                IF sy-subrc NE 0.
                  CONTINUE.
                ENDIF.
              ENDIF.
              LOOP AT lr_scan->tokens INTO ls_tok_scan FROM lv_index WHERE str = lc_comparing.
                DATA(lv_key_ind) = sy-tabix + 1.

                WHILE lv_key_ind <= <lfs_keys>-to.
                  READ TABLE lr_scan->tokens INTO DATA(ls_tokens) INDEX lv_key_ind.
                  IF sy-subrc EQ 0.
                    DATA(lv_idx) = lv_key_ind.
                    lv_key_ind = lv_key_ind + 1.            "+1
                    "delete following 2 lines and assign lv_fld accordingly.
                    lv_fld = |{ lv_fld } { ls_tokens-str }|.

                  ENDIF.
                ENDWHILE.
                EXIT.
              ENDLOOP.
            ENDIF.


            CONDENSE lv_fld.
            "Generate SORT statement

            lv_sort = |{ lv_sort }{ lv_fld } .|.
            CONDENSE lv_sort.
            "Get end line of one statement
            LOOP AT lr_scan->statements ASSIGNING FIELD-SYMBOL(<ls_statements>)
                                                  WHERE from <= lv_index AND
                                                        to   >= lv_index.
              lv_level = <ls_statements>-level.
              DATA(lv_row) = <ls_statements>-trow.
              EXIT.
            ENDLOOP.

*--Read the original source code
            READ TABLE lt_source_map ASSIGNING FIELD-SYMBOL(<ls_source_map>) WITH KEY level = lv_level.
            IF sy-subrc = 0 .
              IF lv_pgm_name <> <ls_source_map>-name.
                DATA(lt_source) = <ls_source_map>-src_code.
                CLEAR lv_counter.
              ENDIF.
              DATA(lt_temp_source) = lt_source.
              lv_pgm_name = <ls_source_map>-name.
*--Read DELETE statment line
              READ TABLE lt_source  "<ls_source_map>-src_code  "changed by sai
                ASSIGNING FIELD-SYMBOL(<ls_src_code>) INDEX lv_row.
              IF sy-subrc = 0.
                DATA(lv_temp_cntr) = lv_row_read - 1.
                DATA: lv_temp_code TYPE string.
                DO 3 TIMES.
                  READ TABLE <ls_source_map>-src_code
                          ASSIGNING <ls_src_code> INDEX lv_temp_cntr.

                  lv_temp_code = |{ <ls_src_code> } { lv_temp_code }|.
                  lv_temp_cntr = lv_temp_cntr - 1.
                ENDDO.

                CONDENSE lv_temp_code.
                TRANSLATE lv_temp_code TO UPPER CASE.
                TRANSLATE lv_sort TO UPPER CASE.
                IF NOT ( lv_temp_code CS lv_sort ).

                  INSERT |{ lv_sort } { gv_comment }|
                         INTO lt_source INDEX lv_counter + lv_row_read.


                  lv_curr_cntr += 1.
                  CALL METHOD me->set_counter
                    EXPORTING
                      im_obj_name  = <lfs_map>-name
                      im_line_no   = lv_row_read + lv_counter
                      im_lines_add = lv_curr_cntr
                      im_org_line  = lv_line
                      im_row       = lv_row_read.
                  lv_counter = lv_counter + 1.
                  ev_insert = abap_true.

                ENDIF.
              ENDIF.
            ENDIF.
            CLEAR : lv_sort,
                    lv_fld,
                    lv_key_ind,
                    ls_tok_scan,
                   " ls_tokens_temp,
                    ls_msg,
                    lv_line.
          ENDIF.
        ENDIF.
*      ENDIF.  "added by sagar
*    ENDLOOP. commented by sai
        IF ev_insert EQ abap_false.
          CONTINUE.
        ENDIF.
*Syntax Check for the updated program : Main program
        READ TABLE lt_source_map ASSIGNING <ls_source_map>
                                        WITH KEY level = lv_level.
        IF sy-subrc EQ 0 .
**--Begin of SJALLIPALL00
**--Validate the modified source code if thre are no errors INSERTthe code.

          IF gv_apply_changes EQ abap_false.
**--Begin of SJALLIPALL00
            LOOP AT <ls_source_map>-src_code ASSIGNING <ls_src_code>
              FROM lv_src_line_from TO lv_src_line_to.
              CONCATENATE lv_o_src <ls_src_code> INTO lv_o_src SEPARATED BY space.
*            APPEND <ls_src_code> TO lt_old_source_code.
            ENDLOOP.

**--BEgin of V2.0
            APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<lfs_result>).
            <lfs_result>-obj_name =   <ls_source_map>-name.
            <lfs_result>-remediation_type = 'DELETE DUPLICATE'.
            <lfs_result>-before = lv_o_src.
            <lfs_result>-line_no = lv_src_line_from.
**--End of V2.0

*          DATA(lv_org_lines) = lines( lt_old_source_code ).
            lv_temp_cntr = lv_counter - 1.
            LOOP AT lt_source ASSIGNING <ls_src_code>
              FROM lv_src_line_from + lv_temp_cntr TO lv_src_line_to + lv_counter.
              CONCATENATE lv_n_src <ls_src_code> INTO lv_n_src SEPARATED BY space.
            ENDLOOP.
            <lfs_result>-after = lv_n_src.
**--End of SJALLIPALL00
          ENDIF.
          CALL METHOD me->meth_validate_src_code
            EXPORTING
              im_object     = <ls_source_map>-name
              im_t_src_code = lt_source
            IMPORTING
              ex_error      = DATA(lv_error).
          IF lv_error EQ abap_false.
            IF gv_apply_changes EQ abap_true. "SAI V2.0
              INSERT REPORT <ls_source_map>-name FROM
              lt_source.
              "<lfs_glb_cntr>-counter = lv_counter.
            ENDIF.      "SAI V2.0
          ELSE.
            lt_source = lt_temp_source.
            lv_counter = lv_counter - lv_curr_cntr .
          ENDIF.
**--End of SJALLIPALL00
        ENDIF.

        CLEAR : lv_sort,
            lv_fld,
            lv_key_ind,
            ls_tok_scan,
            ls_msg,
            lv_line,
            lv_error,
            lv_o_src,
            lv_n_src,
            lv_src_line_to,
            lv_src_line_from,
            lv_curr_cntr.
      ENDLOOP.
      CLEAR lv_counter.
    ENDLOOP.
    me->adjust_counter( ).
  ENDMETHOD.


  METHOD code_wrap.
    DATA: lr_conv     TYPE REF TO cl_wb_abap_source_format,
          lt_src_code TYPE TABLE OF string,
          lt_warnings TYPE synt_errors.

*Insert of Code - Vignesh Sunkasi - Word Wrap
    CREATE OBJECT lr_conv
      EXPORTING
        progname   = im_object
        state      = 'A'
        line_width = 72
      EXCEPTIONS
        not_exists = 1
        OTHERS     = 2.

    IF sy-subrc = 0.
      CALL METHOD lr_conv->convert
        EXPORTING
          truncate_single_lines = space
        IMPORTING
          target_source         = DATA(lt_source_conv)
        EXCEPTIONS
          not_convertable       = 1
          OTHERS                = 2.
      IF sy-subrc = 0.
        CLEAR lt_src_code.

        LOOP AT lt_source_conv ASSIGNING FIELD-SYMBOL(<ls_conv>).
          APPEND INITIAL LINE TO lt_src_code ASSIGNING FIELD-SYMBOL(<ls_src_code>).
          <ls_src_code> = <ls_conv>.
        ENDLOOP.

        INSERT REPORT  im_object FROM lt_src_code.

        " Stntax Check After Insert
        CALL METHOD me->prog_syntax_check
          EXPORTING
            im_object  = im_object
            im_srccode = lt_src_code
          IMPORTING
            et_warning = lt_warnings.

        IF lt_warnings IS INITIAL.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDIF.
*End of Code - Vignesh Sunkasi - Word Wrap
  ENDMETHOD.


  METHOD select_single_remediation.

** Data
    DATA: itab           TYPE STANDARD TABLE OF string,
          lr_scan        TYPE REF TO cl_ci_scan,
          lv_line        TYPE i,
          ls_word        TYPE string,
          ls_dir         TYPE trdir,
          ls_msg         TYPE string,
          lt_warnings    TYPE synt_errors,
          lv_fld         TYPE string,
          lt_source_map  TYPE tt_source_map,
          ls_obj_details TYPE ty_obj_details,
          lin            TYPE i,
          wrd            TYPE string,
          dir            TYPE trdir,
          msg            TYPE string,
          uc             TYPE trdir-uccheck,
          lv_count       TYPE i,
          lv_string      TYPE string,
          lv_string1     TYPE string,
          lv_string2     TYPE string.
    "Declaring global variables
    DATA: lr_scan1   TYPE REF TO cl_ci_scan,
          lr_source1 TYPE REF TO cl_ci_source_include,
          lr_source  TYPE REF TO cl_ci_source_include.

    "Declare internal table and work area
    DATA: lt_token TYPE stokesx_tab.
    DATA: lt_token_where TYPE stokesx_tab.
    DATA: lt_token1 TYPE stokesx_tab.

    "
    DATA: lt_old_Src_code TYPE string_table,
          lt_new_src_code TYPE string_table,
          lv_o_src        TYPE string,
          lv_n_src        TYPE string.

    "Decalring local variable
    DATA: lv_tabix       TYPE sy-tabix,
          lv_tabname     TYPE dcobjdef-name,
          lt_keyfield    TYPE STANDARD TABLE OF cacs_s_cond_keyfields,
          lv_allkey      TYPE c VALUE 'X',
          lt_source_map1 TYPE tt_source_map,
          lv_pgm_name    TYPE string,
          "Variable to capture error message
          lv_flag        TYPE c,
          lv_str1        TYPE string,
          lv_str2        TYPE string,
          lv_curr_cntr   TYPE i.
    "Declaring Constant
    CONSTANTS: lc_info    TYPE char1 VALUE 'I',
               lc_error   TYPE char1 VALUE 'E',
               lc_success TYPE char1 VALUE 'S'.

** Clearing the exporting parameters.
    CLEAR: ev_insert,ex_message.
    gv_seq += 1.
** Code Scan
*    CALL METHOD me->code_scan
*      EXPORTING
*        im_object     = im_object
*      IMPORTING
**       ex_scan       = lr_scan
*        et_source_map = DATA(lt_temp_source_map).

    DATA lt_remediation_obj TYPE SORTED TABLE OF zstr_code_remediation
                                       WITH NON-UNIQUE KEY remediation_type.
    DATA: lt_lines TYPE STANDARD TABLE OF i.
    IF gv_apply_changes IS INITIAL.
      "Code Scan
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = im_object
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = DATA(lt_source_map_temp).

    ELSE.
      lt_source_map_temp = VALUE #( FOR lwa_remed IN gt_remediation WHERE  ( remediation_type = 'SELECT SINGLE' )
                                                    ( name = lwa_remed-obj_name )  ).
      SORT lt_source_map_temp BY name.
      DELETE ADJACENT DUPLICATES FROM lt_source_map_temp COMPARING name.
    ENDIF.

    LOOP AT lt_source_map_temp ASSIGNING FIELD-SYMBOL(<lfs_smap>).
      CALL METHOD me->code_scan
        EXPORTING
          im_object     = <lfs_smap>-name
        IMPORTING
          ex_scan       = lr_scan
          et_source_map = lt_source_map.
** FInd the insert/update/modify etc and check the table name
      DATA(lv_op_index) = 1.
**-->Tokens will store all the keywords including nested includes too.
      "Since it will have all the tokens copy the current level tokens only.
      DATA(lv_level) = lt_source_map[ name = <lfs_smap>-name ]-level.
      DATA(lt_temp_stmnts) = lr_scan->statements.

      SORT lt_temp_stmnts BY level from.
      DATA(lv_token_from) = lt_temp_stmnts[ level = lv_level ]-from.

      SORT lt_temp_stmnts BY level ASCENDING from DESCENDING.
      DATA(lv_token_to) =  lt_temp_stmnts[ level = lv_level ]-to.
      IF gv_apply_changes EQ abap_true.
        CLEAR lt_lines.
*        READ TABLE gt_obj_cntr ASSIGNING FIELD-SYMBOL(<lfs_glb_cntr>)
*                    WITH KEY object_name =  <lfs_smap>-name.
*        IF sy-subrc NE 0.
*          APPEND INITIAL LINE TO gt_obj_cntr ASSIGNING <lfs_glb_cntr>.
*          <lfs_glb_cntr>-object_name =  <lfs_smap>-name.
*        ENDIF.
        lt_lines = VALUE #( FOR lwa_remed IN gt_remediation WHERE  (
                              obj_name = <lfs_smap>-name AND
                              remediation_type = 'SELECT SINGLE'    )
                                 ( lwa_remed-line_no )  ).
      ENDIF.

      WHILE lv_op_index IS NOT INITIAL.

        LOOP AT lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_scan>)
                                FROM lv_op_index
                                WHERE str = 'SELECT'.
          DATA(lv_indx) = sy-tabix.
          IF lv_indx > lv_token_to.
            EXIT.
          ENDIF.
          LOOP AT lr_scan->statements ASSIGNING FIELD-SYMBOL(<lfs_keys>)
                                      WHERE from <= lv_indx AND
                                            to   >= lv_indx.
          ENDLOOP.
          DATA(lv_from) = <lfs_keys>-from.
          DATA(lv_to) = <lfs_keys>-to.
          lv_level = <lfs_keys>-level.                    "SJALLIPALL00
          lv_op_index = <lfs_keys>-to.
          READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token>) INDEX lv_indx + 1.
          IF sy-subrc NE 0 OR <ls_token>-str NE 'SINGLE'.
            CONTINUE.
          ENDIF.
          CLEAR lt_token.
          "getting tocken for the current statements.
          LOOP AT lr_scan->tokens INTO DATA(ls_token)
            FROM  lv_from
            TO  lv_to.
            APPEND ls_token TO lt_token.
          ENDLOOP.
**--Fetching from and to line numbers
          READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_from>) INDEX lv_from.
          IF sy-subrc EQ 0.
            DATA(lv_src_line_from) = <ls_token_from>-row.
          ENDIF.
          READ TABLE lr_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token_to>) INDEX lv_to.
          IF sy-subrc EQ 0.
            DATA(lv_src_line_to) = <ls_token_to>-row.
          ENDIF.
          IF gv_apply_changes EQ abap_true.
            CALL METHOD me->get_counter
              EXPORTING
                im_obj_name = <lfs_smap>-name
                im_line_no  = lv_src_line_from
              IMPORTING
                ex_counter  = DATA(lv_g_cntr).
            "lv_g_cntr = lv_g_cntr - lv_count.
            READ TABLE lt_lines INTO lv_line WITH  KEY  table_line = lv_src_line_from - lv_g_cntr."<lfs_glb_cntr>-counter.
            IF sy-subrc NE 0.
              CONTINUE.
            ENDIF.
          ENDIF.


**--Fetch index of SINGLE token
          READ TABLE lt_token WITH KEY str = 'SINGLE' TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
            lv_indx = sy-tabix + 1.
          ENDIF.

          DATA lv_fields TYPE string.
          CLEAR lv_fields.
**--Find all the fields fetching from the database table
          LOOP AT lt_token INTO DATA(lw_token_3) FROM lv_indx.
            IF lw_token_3-str = 'FROM' OR
               lw_token_3-str = 'INTO'.
              EXIT.
            ENDIF.
            CONCATENATE lv_fields lw_token_3-str INTO lv_fields SEPARATED BY space.
          ENDLOOP.

          "identify the table name to get the key fields
          READ TABLE lt_token TRANSPORTING NO FIELDS WITH KEY str = 'FROM'.
          IF sy-subrc = 0.
            CLEAR lv_tabix.
            lv_tabix = sy-tabix + 1.
            READ TABLE lt_token INTO ls_token INDEX lv_tabix.
            IF sy-subrc = 0  .
              lv_tabname = ls_token-str.
              "Fetch the fkey field for the table
              CALL FUNCTION 'CACS_GET_TABLE_FIELDS'
                EXPORTING
                  i_tabname  = lv_tabname
                TABLES
                  t_keyfield = lt_keyfield.
**--Fetch all the fields checking in where clause
              READ TABLE lt_token TRANSPORTING NO FIELDS WITH KEY str = 'WHERE'.
              IF sy-subrc = 0.
                CLEAR: lv_tabix,
                       lt_token_where.
                lv_tabix = sy-tabix.
                LOOP AT lt_token INTO lw_token_3 FROM lv_tabix.
                  IF lw_token_3-str = 'INTO'.
                    EXIT.
                  ENDIF.
                  APPEND lw_token_3 TO lt_token_where.
                  CLEAR lw_token_3.
                ENDLOOP.
              ENDIF.
**--Check all the key fields are using in where clause or not
              IF NOT lt_keyfield IS INITIAL.
                lv_allkey = 'X'.
                " Ignore first record as it has MANDT as a key field
                LOOP AT lt_keyfield INTO DATA(ls_keyfield) FROM 2.
                  READ TABLE lt_token_where TRANSPORTING NO FIELDS WITH KEY str = ls_keyfield-fieldname.
                  IF sy-subrc <> 0.
                    "All key fields are not used in where clause
                    CLEAR lv_allkey.
                    EXIT.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
**--If atleast one key field is not checking in where clause apply the remediation
          IF lv_allkey IS INITIAL.
            DATA: lt_new_code TYPE TABLE OF string.
            CLEAR lt_new_code.
            DATA(lv_counter) = 0.
            LOOP AT lt_token INTO DATA(lv_data_1).
              DATA(lv_tabix1) = sy-tabix.
              CASE lv_data_1-str.
                WHEN 'SELECT'.
                  APPEND INITIAL LINE TO lt_new_code ASSIGNING FIELD-SYMBOL(<ls_new_code>).
                  <ls_new_code> = |{ 'SELECT' } { lv_fields }|.
                  APPEND INITIAL LINE TO lt_new_code ASSIGNING <ls_new_code>.
                  <ls_new_code> = |{ 'UP TO 1 ROWS' } |.
                WHEN 'FROM'.
                  READ TABLE lt_token INTO lv_data_1 INDEX lv_tabix1 + 1.
                  IF sy-subrc = 0.
                    APPEND INITIAL LINE TO lt_new_code ASSIGNING <ls_new_code>.
                    <ls_new_code> = |{ 'FROM' } { lv_data_1-str }|.
                  ENDIF.

                WHEN 'INTO'.
                  lv_tabix1 = sy-tabix + 1.
                  APPEND INITIAL LINE TO lt_new_code ASSIGNING <ls_new_code>.
                  lv_counter += 1.
                  READ TABLE lt_token INTO lv_data_1 INDEX lv_tabix1.
                  IF sy-subrc = 0.
                    IF lv_data_1-str = 'CORRESPONDING'.
                      lv_tabix1 = lv_tabix1 + 1.
                      READ TABLE lt_token INTO lv_data_1 INDEX lv_tabix1.
                      IF sy-subrc = 0.
                        <ls_new_code> = |{ 'INTO CORRESPONDING' } { lv_data_1-str }|.
                      ENDIF.
                    ELSE.
                      <ls_new_code> = |{ 'INTO' } { lv_data_1-str }|.
                    ENDIF.
                  ENDIF.
                WHEN 'WHERE'.
                  DATA: lv_wh_row TYPE i.
                  CLEAR: lv_fields,
                         lv_wh_row.
                  LOOP AT lt_token_where INTO DATA(lw_token_2).
                    IF lv_wh_row IS INITIAL.
                      lv_wh_row = lw_token_2-row.
                    ENDIF.
                    IF lv_wh_row NE lw_token_2-row.
                      APPEND INITIAL LINE TO lt_new_code ASSIGNING <ls_new_code>.
                      <ls_new_code> = |{ lv_fields }|.
                      CLEAR lv_fields.
                    ENDIF.
                    CONCATENATE lv_fields lw_token_2-str INTO lv_fields SEPARATED BY space.
                  ENDLOOP.
                  IF lv_fields IS NOT INITIAL.
                    APPEND INITIAL LINE TO lt_new_code ASSIGNING <ls_new_code>.
                    <ls_new_code> = |{ lv_fields }|.
                  ENDIF.
                  APPEND INITIAL LINE TO lt_new_code ASSIGNING <ls_new_code>.
                  <ls_new_code> = |{ 'ORDER BY PRIMARY KEY.' }|.
                  APPEND INITIAL LINE TO lt_new_code ASSIGNING <ls_new_code>.
                  <ls_new_code> = |{ 'ENDSELECT.' } { gv_comment }|.
                WHEN OTHERS.
              ENDCASE.
            ENDLOOP.
            READ TABLE lt_source_map ASSIGNING FIELD-SYMBOL(<ls_source_map>) WITH KEY level = lv_level. "1."SJALLIPALL00
            IF sy-subrc IS INITIAL.
              IF lv_pgm_name <> <ls_source_map>-name.
                DATA(lt_source) = <ls_source_map>-src_code.
                CLEAR lv_count.
              ENDIF.
              DATA(lt_temp_src) = lt_source.
              lv_pgm_name = <ls_source_map>-name.
**--Commentthe current query
              DATA(lv_cmnt) = 0.
              LOOP AT lt_source ASSIGNING FIELD-SYMBOL(<ls_src_code>)
                                               FROM lv_src_line_from + lv_count
                                               TO   lv_src_line_to + lv_count.
                CONCATENATE lv_o_src <ls_src_code> INTO lv_o_src SEPARATED BY space. "V2.0
                CONCATENATE '"' <ls_src_code> INTO <ls_src_code>.
                lv_cmnt += 1.
                "APPEND <ls_src_code> TO lt_old_src_code.
              ENDLOOP.
              CALL METHOD me->set_counter
                EXPORTING
                  im_obj_name     = <lfs_smap>-name
                  im_lines_add    = 0
                  im_line_no      = lv_src_line_from + lv_count
                  im_comment_line = lv_cmnt
                  im_row          = lv_src_line_from
                  im_org_line     = lv_line.
              DATA(lv_index) = lv_src_line_to + lv_count.
              DATA(lv_newcode_from) = lv_index.
              " lv_op_index = lv_src_line_to.
**--Insert remediated  query
              CALL METHOD me->set_counter
                EXPORTING
                  im_obj_name  = <lfs_smap>-name
                  im_line_no   = lv_index + 1
                  im_lines_add = lines( lt_new_code )
                  im_row       = lv_src_line_to + 1
                  im_org_line  = lv_line.

              LOOP AT lt_new_code ASSIGNING <ls_new_code>.
                lv_index = lv_index + 1.
                INSERT INITIAL LINE INTO lt_source "<ls_source_map>-src_code
                ASSIGNING <ls_src_code>  INDEX lv_index.
                <ls_src_code> = <ls_new_code>.
                "lv_op_index = lv_index.
                CONCATENATE lv_n_src <ls_new_code> INTO lv_n_src SEPARATED BY space.
              ENDLOOP.

              lv_count = lv_count + lines( lt_new_code ).
              lv_curr_cntr = lines( lt_new_code ).
              DATA(lv_newcode_to) = lv_index.
              DATA(lv_insert) = abap_true.
              DATA(lv_ins_tab) = abap_true.

*            IF gv_apply_changes EQ abap_true.
*              INSERT REPORT  <ls_source_map>-name FROM lt_source.
*            ENDIF.
            ENDIF.
**

            IF lv_ins_tab IS NOT INITIAL.
              EXIT.
            ELSE.
              CLEAR  : lv_src_line_from,lv_src_line_to,lv_newcode_from,
             lv_newcode_to,lt_old_src_code,lt_new_code.
            ENDIF.
          ENDIF.

        ENDLOOP.
        IF sy-subrc NE 0.
          CLEAR lv_op_index.
          EXIT.
        ENDIF.
        CLEAR: lv_indx  .
        IF lv_ins_tab IS NOT INITIAL.
**--Append before and after code for validation purpose
          IF gv_apply_changes EQ abap_false.
            APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<lfs_result>).
            <lfs_result>-obj_name = <ls_source_map>-name.
            <lfs_result>-remediation_type  = 'SELECT SINGLE'.
            <lfs_result>-after =  lv_n_src.
            <lfs_result>-before = lv_o_src.
            <lfs_result>-line_no = lv_src_line_from.
          ENDIF.
*          IF gv_apply_changes EQ abap_true.
*            INSERT REPORT  <ls_source_map>-name FROM lt_source.
*          ENDIF.
          CLEAR lv_ins_tab.

          READ TABLE lt_source_map ASSIGNING <ls_source_map> WITH KEY level = <lfs_keys>-level.
          IF sy-subrc EQ 0.
**--Begin of SJALLIPALL00
**--Validate the modified source code if thre are no errors INSERTthe code.
            CALL METHOD me->meth_validate_src_code
              EXPORTING
                im_object     = <ls_source_map>-name
                im_t_src_code = lt_source
              IMPORTING
                ex_error      = DATA(lv_error).
            IF lv_error EQ abap_false." AND
              IF   gv_apply_changes EQ abap_true. "SAI V2.0
                lv_counter = lv_counter + lines( lt_new_code ).
                INSERT REPORT <ls_source_map>-name FROM lt_source.
              ENDIF.      "SAI V2.0
            ELSE.
              lt_source = lt_temp_src.
              lv_count =  lv_count - lv_curr_cntr.
            ENDIF.
**--End of SJALLIPALL00
** Syntax Check for the updated program
*          CALL METHOD me->prog_syntax_check
*            EXPORTING
*              im_object  = <ls_source_map>-name "im_object
*              im_srccode = <ls_source_map>-src_code
*            IMPORTING
*              et_warning = lt_warnings
*              ex_line    = lv_line
*              ex_word    = ls_word
*              ex_msg     = ls_msg
*              ex_dir     = ls_dir.
**                  IF lv_line NE 0 AND ls_msg IS NOT INITIAL.
*          IF lt_warnings IS NOT INITIAL.
*            LOOP AT lt_warnings ASSIGNING FIELD-SYMBOL(<fs_warning>).
*              APPEND INITIAL LINE TO ex_message ASSIGNING FIELD-SYMBOL(<lfs_message>).
*              <lfs_message>-line = <fs_warning>-line.
*              <lfs_message>-message = <fs_warning>-message.
*              <lfs_message>-typ = 'E'.
*            ENDLOOP.
*            CLEAR : ev_insert.
*          ELSE.
*            APPEND INITIAL LINE TO ex_message ASSIGNING <lfs_message>.
*            <lfs_message>-typ = 'S'.
*            <lfs_message>-message = TEXT-000.
*            COMMIT WORK AND WAIT.
*          ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: lv_n_src,lv_o_src,lv_src_line_from,lv_src_line_to,lt_warnings,lv_curr_cntr.
      ENDWHILE.
      ev_insert = lv_insert.
*      IF <lfs_glb_cntr> IS ASSIGNED.
*        <lfs_glb_cntr>-counter =  <lfs_glb_cntr>-counter + lv_counter.
*      ENDIF.
    ENDLOOP.
    me->adjust_counter( ).
  ENDMETHOD.


  METHOD constructor.
    gv_comment = add_comments( ).
  ENDMETHOD.


  METHOD process_application_log.
    DATA:
      gs_log              TYPE bal_s_log,
      gs_msg              TYPE bal_s_msg,
      gt_handle           TYPE bal_t_logh,
      gs_handle           LIKE LINE OF gt_handle,
*      gs_context          TYPE zca_s_appllog,         " type of your z-structure
      l_s_display_profile TYPE bal_s_prof,
      l_s_fcat            TYPE bal_s_fcat.


* create log entry
    gs_log-object    = 'ZREMEDIATION'. " All available logs are in BALSUB table, use
    " TCode SLG0 for maintenance
    gs_log-subobject = 'ZSYNTAXCHK'.
    gs_log-extnumber = im_object.
    gs_log-aluser    = sy-uname.
    gs_log-alprog    = im_object.
    .

* Create new log in logs storage area
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = gs_log
      IMPORTING
        e_log_handle = gs_handle.
    APPEND gs_handle TO gt_handle.
    READ TABLE ex_message ASSIGNING FIELD-SYMBOL(<fs_message>) WITH KEY typ = 'E'.
    IF sy-subrc EQ 0.
      CLEAR gs_msg.
      gs_msg-msgid = 'SY'.
      gs_msg-msgno = '047'.
      gs_msg-msgv1 = im_object.

* Save message
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_s_msg      = gs_msg
          i_log_handle = gs_handle.

    ENDIF.
* adding random message

    APPEND LINES OF ex_message TO gt_messages.
    SORT gt_messages BY typ message.
    DELETE ADJACENT DUPLICATES FROM gt_messages COMPARING typ message.
*    LOOP AT ex_message  ASSIGNING <fs_message>.
    LOOP AT gt_messages  ASSIGNING <fs_message>.
      CLEAR gs_msg.
      gs_msg-msgty = <fs_message>-typ.
      gs_msg-msgid = 'ZREMEDIATION'.
      IF <fs_message>-typ = 'E'.
        gs_msg-msgno = '001'.
        gs_msg-msgv1 = <fs_message>-line.
        gs_msg-msgv2 = <fs_message>-message.
      ELSE.
        gs_msg-msgno = '000'.
        gs_msg-msgv1 = <fs_message>-message.
      ENDIF.

* Save message
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_s_msg      = gs_msg
          i_log_handle = gs_handle.

    ENDLOOP.

    CLEAR gs_msg.

    gs_msg-msgty = <fs_message>-typ.
    gs_msg-msgid = 'ZREMEDIATION'.
    gs_msg-msgno = '000'.
    gs_msg-msgv1 = 'Log Changes :'."<fs_message>-line'.
    gs_msg-msgv2 = <fs_message>-message.
* Save message
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg      = gs_msg
        i_log_handle = gs_handle.
    LOOP AT gt_log_messages  ASSIGNING FIELD-SYMBOL(<lfs_app_msg>).
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_s_msg      = <lfs_app_msg>
          i_log_handle = gs_handle.
    ENDLOOP.


    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle = gt_handle.


    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD get_obj_detail.
    CLEAR: es_obj_details.

    es_obj_details-transport_number = im_transportno.
    es_obj_details-development_class = im_package.

    IF im_object IS NOT INITIAL.
      SELECT SINGLE name,
                    subc,
                    appl,
                    secu
        FROM trdir
        INTO @DATA(ls_trdir)
        WHERE name = @im_object.

      es_obj_details-application = ls_trdir-appl.
      es_obj_details-authorization_group = ls_trdir-secu.
      es_obj_details-program_type = ls_trdir-subc.
      es_obj_details-program_name = ls_trdir-name.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE name,
                      text
          FROM trdirt
          INTO @DATA(ls_trdirt)
          WHERE name = @im_object
          AND sprsl = @sy-langu.
        IF sy-subrc IS INITIAL.
          es_obj_details-title_string = ls_trdirt-text.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
