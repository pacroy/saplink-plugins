class ZSAPLINK_TRANSACTIONS definition
  public
  inheriting from ZSAPLINK
  final
  create public .

public section.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.

  constants C_HEX_TRA type CHAR2 value '00' ##NO_TEXT.
  constants C_HEX_MEN type CHAR2 value '01' ##NO_TEXT.
  constants C_HEX_PAR type CHAR2 value '02' ##NO_TEXT.
  constants C_HEX_REP type CHAR2 value '80' ##NO_TEXT.
  constants C_HEX_RPV type CHAR2 value '10' ##NO_TEXT.
  constants C_HEX_OBJ type CHAR2 value '08' ##NO_TEXT.
  constants C_HEX_CHK type CHAR2 value '04' ##NO_TEXT.
  constants C_HEX_ENQ type CHAR2 value '20' ##NO_TEXT.
  constants C_OO_PROGRAM type CHAR9 value '\PROGRAM=' ##NO_TEXT.
  constants C_OO_CLASS type CHAR7 value '\CLASS=' ##NO_TEXT.
  constants C_OO_METHOD type STRING value '\METHOD=' ##NO_TEXT.
  constants C_OO_TCODE type STRING value 'OS_APPLICATION' ##NO_TEXT.
  constants C_OO_FRCLASS type STRING value 'CLASS' ##NO_TEXT.
  constants C_OO_FRMETHOD type STRING value 'METHOD' ##NO_TEXT.
  constants C_OO_FRUPDTASK type STRING value 'UPDATE_MODE' ##NO_TEXT.
  constants C_OO_SYNCHRON type STRING value 'S' ##NO_TEXT.
  constants C_OO_ASYNCHRON type STRING value 'U' ##NO_TEXT.
  constants C_OO_LOKAL type C value 'L' ##NO_TEXT.
  constants C_TRUE type C value 'X' ##NO_TEXT.
  constants C_FALSE type C value SPACE ##NO_TEXT.

  class-methods SPLIT_PARAMETERS
    exporting
      !ET_RSPARAM type S_PARAM
      !ES_RSSTCD type RSSTCD
    changing
      !IS_TSTCP type TSTCP
      !ES_TSTC type TSTC .
  class-methods SPLIT_PARAMETERS_COMP
    importing
      !IV_TYPE type ANY
      !IV_PARAM type ANY
    changing
      !IC_VALUE type ANY .

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
ENDCLASS.



CLASS ZSAPLINK_TRANSACTIONS IMPLEMENTATION.


METHOD checkexists.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

  DATA: ls_tstc  TYPE tstc,
        l_tcode  TYPE tstc-tcode.

  CLEAR  exists.

  l_tcode =  objname.

* Check transaction code already exists.
  SELECT SINGLE *
    INTO ls_tstc
    FROM tstc
   WHERE tcode EQ l_tcode.

  IF sy-subrc EQ 0.
    exists = 'X'.
  ENDIF.

ENDMETHOD.


METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

  DATA: l_tcode      TYPE tstc-tcode,
        _objtype     TYPE string,
        mtext        type string,
        rc           TYPE sysubrc.

* XML nodes
  DATA:rootnode    TYPE REF TO if_ixml_element,
       tstc_node   TYPE REF TO if_ixml_element,
       tstct_node  TYPE REF TO if_ixml_element,
       tstcc_node  TYPE REF TO if_ixml_element,
       tstcp_node  TYPE REF TO if_ixml_element,
       rsstcd_node TYPE REF TO if_ixml_element,
       rsparam_node TYPE REF TO if_ixml_element.

* Data Nodes
  DATA:
        lt_tstc    TYPE STANDARD TABLE OF tstc,
        ls_tstc    TYPE                   tstc,
        lt_tstcc   TYPE STANDARD TABLE OF tstcc,
        ls_tstcc   TYPE                   tstcc,
        ls_tstcp   TYPE                   tstcp,
        ls_tstct   TYPE                   tstct.

  l_tcode = objname.

* Read transaction details
  CALL FUNCTION 'RPY_TRANSACTION_READ'
   EXPORTING
     transaction            = l_tcode
*   PROGRAM                =
*   DYNPRO                 =
*   TRANSACTION_TYPE       = ' '
   TABLES
      tcodes                 = lt_tstc
      gui_attributes         = lt_tstcc
   EXCEPTIONS
     permission_error       = 1
     cancelled              = 2
     not_found              = 3
     object_not_found       = 4
     OTHERS                 = 5
            .
  IF sy-subrc <> 0.

    CASE sy-subrc.

      WHEN 1.
        RAISE EXCEPTION TYPE zcx_saplink
         EXPORTING textid = zcx_saplink=>not_authorized.

      WHEN 3 OR 4.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_found.

      WHEN OTHERS.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                INTO mtext
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING msg = mtext.

    ENDCASE.
  ENDIF.

  CLEAR: ls_tstc,
         ls_tstcc.
  READ TABLE lt_tstc INTO ls_tstc WITH KEY tcode = l_tcode.
  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>not_found.
  ENDIF.

  READ TABLE lt_tstcc INTO ls_tstcc WITH KEY tcode = l_tcode.
  IF sy-subrc NE 0.
  ENDIF.

* Get short text
  SELECT SINGLE *
    INTO ls_tstct
    FROM tstct
   WHERE sprsl = sy-langu
     AND tcode = l_tcode.

* Get parameters
  SELECT SINGLE *
  INTO ls_tstcp
  FROM tstcp
  WHERE tcode = l_tcode.

  IF sy-subrc NE 0.
*  Do nothing as not all transactions
*  need have parameters
  ENDIF.

  DATA: ls_t_tstcp TYPE tstcp,
        lt_rsparam TYPE s_param,
        ls_rsparam TYPE rsparam,
        ls_rsstcd  TYPE rsstcd.

  ls_t_tstcp = ls_tstcp.

* Split parameters
  CALL METHOD zsaplink_transactions=>split_parameters
    IMPORTING
      et_rsparam = lt_rsparam
      es_rsstcd  = ls_rsstcd
    CHANGING
      is_tstcp   = ls_t_tstcp
      es_tstc    = ls_tstc.

  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = ls_tstc ).

  tstct_node = xmldoc->create_element( 'tstct' ).
  setattributesfromstructure( node = tstct_node structure = ls_tstct ).
  rc = rootnode->append_child( tstct_node ).

  tstcc_node = xmldoc->create_element( 'tstcc' ).
  setattributesfromstructure( node = tstcc_node structure = ls_tstcc ).
  rc = rootnode->append_child( tstcc_node ).

  tstcp_node = xmldoc->create_element( 'tstcp' ).
  setattributesfromstructure( node = tstcp_node structure = ls_tstcp ).
  rc = rootnode->append_child( tstcp_node ).

  rsstcd_node = xmldoc->create_element( 'rsstcd' ).
  setattributesfromstructure( node = rsstcd_node structure = ls_rsstcd ).
  rc = rootnode->append_child( rsstcd_node ).

  LOOP AT lt_rsparam INTO ls_rsparam.
    rsparam_node = xmldoc->create_element( 'rsparam' ).
    setattributesfromstructure( node = rsparam_node structure = ls_rsparam ).
    rc = rootnode->append_child( rsparam_node ).
  ENDLOOP.

  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.

ENDMETHOD.


METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

  DATA: l_tcode      TYPE tstc-tcode,
        _objtype     TYPE string,
        l_tran_type  TYPE rglif-docutype,
        mtext        TYPE string,
        rc           TYPE sysubrc,
        l_easy_web_transaction TYPE c,
        l_html_enabled  TYPE c.


* XML nodes
  DATA:rootnode    TYPE REF TO if_ixml_element,
       tstc_node   TYPE REF TO if_ixml_element,
       tstct_node  TYPE REF TO if_ixml_element,
       tstcc_node  TYPE REF TO if_ixml_element,
       tstcp_node  TYPE REF TO if_ixml_element,
       rsstcd_node TYPE REF TO if_ixml_element,
       rsparam_node TYPE REF TO if_ixml_element.


  DATA : node        TYPE REF TO if_ixml_element,
         filter      TYPE REF TO if_ixml_node_filter,
         iterator    TYPE REF TO if_ixml_node_iterator.

* Data Nodes
  DATA:
        ls_tstc    TYPE tstc,
        ls_tstcc   TYPE tstcc,
        ls_tstcp   TYPE tstcp,
        ls_tstct   TYPE tstct,
        lt_rsparam TYPE s_param,
        ls_rsparam TYPE rsparam,
        ls_rsstcd  TYPE rsstcd.

  DATA  l_checkexists TYPE flag.

  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = ls_tstc.

  objname = ls_tstc-tcode.

  l_checkexists = checkexists( ).
  IF l_checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

  tstct_node = xmldoc->find_from_name( 'tstct' ).
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = tstct_node
    CHANGING
      structure = ls_tstct.


  tstcc_node = xmldoc->find_from_name( 'tstcc' ).
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = tstcc_node
    CHANGING
      structure = ls_tstcc.

  tstcp_node = xmldoc->find_from_name( 'tstcp' ).
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = tstcp_node
    CHANGING
      structure = ls_tstcp.

  rsstcd_node = xmldoc->find_from_name( 'rsstcd' ).
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rsstcd_node
    CHANGING
      structure = ls_rsstcd.


  FREE: filter, iterator, rsparam_node.
  filter = xmldoc->create_filter_name( 'rsparam' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  rsparam_node ?= iterator->get_next( ).
  WHILE rsparam_node IS NOT INITIAL.
    CLEAR ls_rsparam.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rsparam_node
      CHANGING
        structure = ls_rsparam.
    APPEND ls_rsparam TO lt_rsparam.
    rsparam_node ?= iterator->get_next( ).
  ENDWHILE.

  l_tcode = objname.

  CASE ls_tstc-cinfo.

    WHEN c_hex_tra.
      l_tran_type = ststc_c_type_dialog.
    WHEN c_hex_rep.
      l_tran_type =  ststc_c_type_report.
    WHEN c_hex_par.
      IF  ls_rsstcd-s_vari EQ c_true.
        l_tran_type = ststc_c_type_variant.
      ELSE.
        l_tran_type = ststc_c_type_parameters.
      ENDIF.

    WHEN c_hex_obj.
      l_tran_type = ststc_c_type_object.
    WHEN OTHERS.

  ENDCASE.

  CASE ls_tstcc-s_webgui.
    WHEN '1'.
      l_html_enabled = c_true.
    WHEN '2'.
      l_easy_web_transaction = c_true.
    WHEN OTHERS.
      CLEAR: l_easy_web_transaction,
             l_html_enabled.
  ENDCASE.

  DATA: l_dnum TYPE d020s-dnum.
  l_dnum = ls_tstc-dypno.
  CALL FUNCTION 'RPY_TRANSACTION_INSERT'
    EXPORTING
      transaction                         = l_tcode
      program                             = ls_tstc-pgmna
      dynpro                              = l_dnum
      language                            = ls_tstct-sprsl
*   WITH_DOCU                           = ' '
*   DOCUTYPE                            = 'T'
*   DEVELOPMENT_CLASS                   = '$TMP'
*   TRANSPORT_NUMBER                    =
    transaction_type                    =  l_tran_type
      shorttext                           = ls_tstct-ttext
   called_transaction                  = ls_rsstcd-call_tcode
   called_transaction_skip             = ls_rsstcd-st_skip_1
   variant                             = ls_rsstcd-variant
   cl_independend                      = ls_rsstcd-s_ind_vari
    easy_web_transaction                = l_easy_web_transaction
*   PROFESSIONEL_USER_TRANSACTION       =
    html_enabled                        =  l_html_enabled
    java_enabled                        = ls_tstcc-s_platin
    wingui_enabled                      = ls_tstcc-s_win32
    servicefile                         = ls_tstcc-s_service
*   GENFLAG                             = ' '
*   SUPPRESS_AUTHORITY_CHECK            = ' '
*   SUPPRESS_CORR_INSERT                = ' '
 TABLES
*   DOCU_TABLE_USER                     =
*   DOCU_TABLE_TECH                     =
    param_values                        = lt_rsparam
 EXCEPTIONS
   cancelled                           = 1
   already_exist                       = 2
   permission_error                    = 3
   name_not_allowed                    = 4
   name_conflict                       = 5
   illegal_type                        = 6
   object_inconsistent                 = 7
   db_access_error                     = 8
   OTHERS                              = 9
            .
  IF sy-subrc <> 0.

    CASE sy-subrc.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>existing.

      WHEN 3.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_authorized.

      WHEN OTHERS.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                INTO mtext
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING msg = mtext.
    ENDCASE.

  ENDIF.
  name = l_tcode.

ENDMETHOD.


METHOD deleteobject.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

  DATA: l_tcode TYPE tstc-tcode,
        mtext        TYPE string.

  l_tcode = objname.
  CALL FUNCTION 'RPY_TRANSACTION_DELETE'
    EXPORTING
      transaction                    = l_tcode
*   TRANSPORT_NUMBER               =
*   SUPPRESS_AUTHORITY_CHECK       = ' '
*   SUPPRESS_CORR_INSERT           = ' '
   EXCEPTIONS
     not_excecuted                  = 1
     object_not_found               = 2
     OTHERS                         = 3
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            INTO mtext
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING msg = mtext.
  ENDIF.


ENDMETHOD.


method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

  objecttype = 'TRAN'.  "Transactions
endmethod.


METHOD split_parameters.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com


*      copied from SAP standard program LSEUKF01
*      form Split_parameters to interpret the parameter
*      details of the transaction


  FIELD-SYMBOLS <f> TYPE ANY.
  DATA: off TYPE i,
        param_beg TYPE i,
        l_length TYPE i.

  DATA: ls_param TYPE rsparam.

  REFRESH et_rsparam.
  CLEAR ls_param.


  IF is_tstcp-param(1) = '\'.             " OO-Transaktion ohne FR

    CALL METHOD zsaplink_transactions=>split_parameters_comp
      EXPORTING
        iv_type  = c_oo_program
        iv_param = is_tstcp-param
      CHANGING
        ic_value = es_tstc-pgmna.

    CALL METHOD zsaplink_transactions=>split_parameters_comp
      EXPORTING
        iv_type  = c_oo_class
        iv_param = is_tstcp-param
      CHANGING
        ic_value = es_rsstcd-classname.

    CALL METHOD zsaplink_transactions=>split_parameters_comp
      EXPORTING
        iv_type  = c_oo_method
        iv_param = is_tstcp-param
      CHANGING
        ic_value = es_rsstcd-method.

    IF NOT es_tstc-pgmna IS INITIAL.
      es_rsstcd-s_local = c_true.
    ENDIF.
    EXIT.
  ELSEIF is_tstcp-param(1) = '@'.         " Transaktionsvariante
    es_rsstcd-s_vari = c_true.
    IF is_tstcp-param(2) = '@@'.
      es_rsstcd-s_ind_vari = c_true. off = 2.
    ELSE.
      CLEAR es_rsstcd-s_ind_vari. off = 1.
    ENDIF.
    IF is_tstcp-param CA ' '. ENDIF.
    sy-fdpos = sy-fdpos - off.
    es_rsstcd-call_tcode = is_tstcp-param+off(sy-fdpos).
    sy-fdpos = sy-fdpos + 1 + off.
    es_rsstcd-variant = is_tstcp-param+sy-fdpos.
  ELSEIF is_tstcp-param(1) = '/'.
    es_rsstcd-st_tcode = c_true.
    es_rsstcd-st_prog  = space.
    IF is_tstcp-param+1(1) = '*'.
      es_rsstcd-st_skip_1 = c_true.
    ELSE.
      CLEAR es_rsstcd-st_skip_1.
    ENDIF.
    IF is_tstcp-param CA ' '. ENDIF.
    param_beg = sy-fdpos + 1.
    SUBTRACT 2 FROM sy-fdpos.
    IF sy-fdpos GT 0.
      es_rsstcd-call_tcode = is_tstcp-param+2(sy-fdpos).
    ENDIF.
*    select single * from tstc into *tstc
*                    where tcode = rsstcd-call_tcode.
*    if sy-subrc = 0.
*      IF *tstc-cinfo O hex_rep.
*        PERFORM fill_tfields_report USING *tstc-pgmna.
*      ELSEIF *tstc-cinfo O hex_men OR *tstc-cinfo O hex_par.
*      ELSE.
*        PERFORM fill_tfields_dynpro USING *tstc-pgmna *tstc-dypno.
*      ENDIF.
*    ENDIF.
    SHIFT is_tstcp-param BY param_beg PLACES.
  ELSE.
    es_rsstcd-st_tcode = space.
    es_rsstcd-st_prog  = c_true.
*    PERFORM fill_tfields_dynpro USING tstc-pgmna tstc-dypno.
  ENDIF.

  DO 254 TIMES.
    IF is_tstcp-param = space. EXIT. ENDIF.
    CLEAR ls_param.
*    condense is_tstcp-param no-gaps.
    IF is_tstcp-param CA '='.
      CHECK sy-fdpos NE 0.
      ASSIGN is_tstcp-param(sy-fdpos) TO <f>.
      ls_param-field = <f>.
      IF ls_param-field(1) = space. SHIFT  ls_param-field. ENDIF.
      sy-fdpos = sy-fdpos + 1.
      SHIFT is_tstcp-param BY sy-fdpos PLACES.
      IF is_tstcp-param CA ';'.
        IF sy-fdpos NE 0.
          ASSIGN is_tstcp-param(sy-fdpos) TO <f>.
          ls_param-value = <f>.
          IF ls_param-value(1) = space. SHIFT  ls_param-value. ENDIF.
        ENDIF.
        sy-fdpos = sy-fdpos + 1.
        SHIFT is_tstcp-param BY sy-fdpos PLACES.
        APPEND ls_param TO et_rsparam.
      ELSE.       " Da _____; möglich
        l_length = STRLEN( is_tstcp-param ).
        CHECK l_length > 0.
        ASSIGN is_tstcp-param(l_length) TO <f>.
        ls_param-value = <f>.
        IF ls_param-value(1) = space. SHIFT  ls_param-value. ENDIF.
        ADD 1 TO l_length.
        SHIFT is_tstcp-param BY l_length PLACES.
        APPEND ls_param TO et_rsparam.
      ENDIF.
    ENDIF.
  ENDDO.
* oo-Transaktion mit Framework
  IF es_rsstcd-call_tcode = c_oo_tcode.
    es_rsstcd-s_trframe = c_true.
    LOOP AT et_rsparam INTO ls_param.
      CASE ls_param-field.
        WHEN c_oo_frclass. es_rsstcd-classname = ls_param-value.
        WHEN c_oo_frmethod. es_rsstcd-method   = ls_param-value.
        WHEN c_oo_frupdtask.
          IF ls_param-value = c_oo_synchron.
            es_rsstcd-s_upddir  = c_true.
            es_rsstcd-s_updtask = c_false.
            es_rsstcd-s_updlok  = c_false.
          ELSEIF ls_param-value = c_oo_asynchron.
            es_rsstcd-s_upddir  = c_false.
            es_rsstcd-s_updtask = c_true.
            es_rsstcd-s_updlok  = c_false.
          ELSE.
            es_rsstcd-s_upddir  = c_false.
            es_rsstcd-s_updtask = c_false.
            es_rsstcd-s_updlok  = c_true.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD split_parameters_comp.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com


  DATA: off TYPE i.

  IF iv_param CS iv_type.
    off = sy-fdpos + STRLEN( iv_type ).
    ic_value = iv_param+off.
    IF ic_value CA '\'.
      CLEAR ic_value+sy-fdpos.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
