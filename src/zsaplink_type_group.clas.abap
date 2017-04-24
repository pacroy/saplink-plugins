class ZSAPLINK_TYPE_GROUP definition
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

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.

  data TP_STATE type DDOBJSTATE .
ENDCLASS.



CLASS ZSAPLINK_TYPE_GROUP IMPLEMENTATION.


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
* Plugin created by:
*   Dusan Davidovic
*   DusanDavidovic@gmail.com


  DATA: w_typepool TYPE typegroup.

  w_typepool = me->objname.

  TRY .
      CALL FUNCTION 'TYPD_GET_STATE_FOR_SE11'
        EXPORTING
          typdname       = w_typepool
        IMPORTING
          state          = me->tp_state
        EXCEPTIONS
          reps_not_exist = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CLEAR exists.
      ELSE.
        exists = 'X'.
      ENDIF.
    CATCH cx_sy_dyn_call_illegal_func .
      DATA: w_prog TYPE syrepid.
      CONCATENATE '%_C' me->objname INTO w_prog.
      SELECT SINGLE state INTO me->tp_state
        FROM progdir
       WHERE name = w_prog
         AND state = 'A'.
      IF sy-subrc <> 0.
        CLEAR exists.
      ELSE.
        exists = 'X'.
      ENDIF.


  ENDTRY.


ENDMETHOD.


method CREATEIXMLDOCFROMOBJECT.
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
* Plugin created by:
*   Dusan Davidovic
*   DusanDavidovic@gmail.com

  DATA:
         wt_smodisrc TYPE  standard table of  smodisrc with default key,
         wt_smodilog TYPE  standard table of  smodilog with default key,
         wt_source   TYPE  standard table of  abaptxt255 with default key,
         wt_trdir    TYPE  standard table of 	trdir with default key,
         w_typepool  TYPE typegroup.

  IF me->checkexists( ) IS NOT INITIAL. " this will set type pool state
* get type pool information
    w_typepool = me->objname.
    CALL FUNCTION 'TYPD_GET_OBJECT'
      EXPORTING
        typdname          = w_typepool
        r3state           = me->tp_state
      TABLES
        psmodisrc         = wt_smodisrc
        psmodilog         = wt_smodilog
        psource           = wt_source
        ptrdir            = wt_trdir
      EXCEPTIONS
        version_not_found = 1
        reps_not_exist    = 2
        OTHERS            = 3.
    IF sy-subrc = 0.
* Create XML
      DATA: rootnode   TYPE REF TO if_ixml_element,
            textnode   TYPE REF TO if_ixml_element,
            sourcenode TYPE REF TO if_ixml_element.
      DATA: ws_objtype TYPE string,
            ws_ddtypet type ddtypet,
            tp_attr    TYPE tadir,
            ws_trdir   TYPE trdir,
            sourcestring TYPE string.
      DATA rc TYPE sysubrc.


      ws_objtype = getobjecttype( ).
      rootnode = xmldoc->create_element( ws_objtype ). " TYPE
      SELECT SINGLE * FROM tadir INTO tp_attr
       WHERE pgmid   = 'R3TR'
         AND object  = ws_objtype
         AND obj_name = objname.
      me->setattributesfromstructure( node      = rootnode
                                      structure =  tp_attr ).
* set text node
      textnode = xmldoc->create_element( 'text' ).
      SELECT SINGLE * FROM ddtypet INTO ws_ddtypet WHERE typegroup = w_typepool.
      me->setattributesfromstructure( node      = textnode
                                      structure = ws_ddtypet ).
      rc = rootnode->append_child( textnode ).
* source node
      sourcenode = xmldoc->create_element( 'source' ).
      READ TABLE wt_trdir INTO ws_trdir INDEX 1.
      me->setattributesfromstructure( node      = sourcenode
                                      structure =  ws_trdir ).
      DATA: wt_src TYPE rswsourcet.
      FIELD-SYMBOLS: <at> TYPE  abaptxt255.
      LOOP AT wt_source ASSIGNING <at> .
        APPEND <at> TO wt_src.
      ENDLOOP.
      sourcestring = me->buildsourcestring( sourcetable = wt_src ).
      rc = sourcenode->if_ixml_node~set_value( sourcestring ).

      rc = rootnode->append_child( sourcenode ).
      rc = xmldoc->append_child( rootnode ).
      ixmldocument = xmldoc.
    ENDIF.
  ELSE.
    CLEAR ixmldocument.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>not_found.
  ENDIF.
endmethod.


method CREATEOBJECTFROMIXMLDOC.
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
* Plugin created by:
*   Dusan Davidovic
*   DusanDavidovic@gmail.com

  DATA: w_devclass TYPE devclass,
        w_typepool TYPE typegroup,
        w_objtype  TYPE string,
        rc TYPE sysubrc.
* xml nodes
  DATA: rootnode   TYPE REF TO if_ixml_element,
        textnode   TYPE REF TO if_ixml_element,
        sourcenode TYPE REF TO if_ixml_element.
* structures
  DATA:  ws_ddtypet TYPE ddtypet,
         ws_tadir   TYPE tadir,
         ws_trdir   TYPE trdir.

  w_objtype = me->getobjecttype( ).
  me->xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( w_objtype ).
  me->getstructurefromattributes( EXPORTING node = rootnode
                                  CHANGING  structure = ws_tadir ).
* check if object exist
  me->objname = ws_tadir-obj_name.
  IF me->checkexists( ) IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

* get text
  textnode = rootnode->find_from_name( 'text' ).
  me->getstructurefromattributes( EXPORTING node = textnode
                                  CHANGING  structure = ws_ddtypet ).
* get source
  DATA: wt_abapsource TYPE STANDARD TABLE OF abapsource,
        ws_abapsource TYPE abapsource,
        wt_sourcestring TYPE STANDARD TABLE OF string,
        w_sourcestring TYPE string.
  sourcenode = rootnode->find_from_name( 'source' ).
  me->getstructurefromattributes( EXPORTING node = sourcenode
                                  CHANGING  structure = ws_trdir ).
  w_sourcestring = sourcenode->get_value( ).
  wt_sourcestring = me->buildtablefromstring( w_sourcestring ).
  LOOP AT wt_sourcestring INTO w_sourcestring .
    ws_abapsource-line = w_sourcestring.
    APPEND ws_abapsource TO wt_abapsource.
  ENDLOOP.


* create Type-Pool
  DATA: w_corrnum TYPE trkorr.
  w_typepool = me->objname.
  CALL FUNCTION 'RS_DD_TYGR_INSERT_SOURCES'
    EXPORTING
      typegroupname        = w_typepool
      ddtext               = ws_ddtypet-ddtext
      corrnum              = w_corrnum
      devclass             = w_devclass
    TABLES
      SOURCE               = wt_abapsource
    EXCEPTIONS
      already_exists       = 1
      not_executed         = 2
      permission_failure   = 3
      object_not_specified = 4
      illegal_name         = 5
      OTHERS               = 6.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    CASE sy-subrc .
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING textid = zcx_saplink=>existing.
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_authorized.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>system_error.
    ENDCASE.
  ELSE.
* update unicode flag if required....TRDIR
* Use FM's RS_TRDIR_SELECT and RS_TRDIR_UPDATE
    IF ws_trdir-uccheck IS NOT INITIAL.
* this is required to update UNICODE flag - UCCHECK
* Behavior is like this...
* Two records are created in PROGDIR, one for (A)ctive and one for (I)nactive version,
* active ones has unicode set ptoperly while inacative one is blank. When type group is activated,
* active record is deleted and for inactive one status is changed, therefore UCCHECK is not corect.
* I suspect this behavior is caused by FM RS_DD_TYGR_INSERT_SOURCES where unicode can not be set
* Note: TRDIR is also updated
      DATA: wt_progdir_tab TYPE STANDARD TABLE OF progdir.
      CALL FUNCTION 'READ_PROGDIR'
        EXPORTING
          i_progname    = ws_trdir-name
          i_state       = ' '
        IMPORTING
          e_progdir_tab = wt_progdir_tab
        EXCEPTIONS
          not_exists    = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>system_error.
      ELSE.
        FIELD-SYMBOLS: <pdir> TYPE progdir.
        LOOP AT wt_progdir_tab ASSIGNING  <pdir> WHERE uccheck IS INITIAL.
          <pdir>-uccheck = ws_trdir-uccheck.
          CALL FUNCTION 'UPDATE_PROGDIR'
            EXPORTING
              i_progdir    = <pdir>
              i_progname   = ws_trdir-name
              i_state      = <pdir>-state
            EXCEPTIONS
              not_executed = 1
              OTHERS       = 2.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_saplink
              EXPORTING textid = zcx_saplink=>error_message
                msg = 'PROGDIR not updated'.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    name = w_typepool.
  ENDIF.

endmethod.


method DELETEOBJECT.
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
* Plugin created by:
*   Dusan Davidovic
*   DusanDavidovic@gmail.com

  DATA: w_typepool TYPE typegroup,
        w_stringobjtype TYPE string,
        w_fmobjtype TYPE trobjtype,
        w_eutype TYPE ddeutype.

  w_typepool = me->objname.
  w_stringobjtype  = me->getobjecttype( ).
  w_fmobjtype = w_stringobjtype.


  CALL FUNCTION 'INTERN_TRANSL_TADIR_TYPE'
    EXPORTING
      tadir_type   = w_fmobjtype
    IMPORTING
      eutype       = w_eutype
    EXCEPTIONS
      invalid_type = 1
      OTHERS       = 2.
  IF sy-subrc = 0.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = 'X'
        objname              = w_typepool
        objtype              = w_eutype
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4
        dialog_needed        = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
   EXPORTING
     textid = zcx_saplink=>error_message
     msg = 'type-pool not deleted'.
    ENDIF.

  ENDIF.

endmethod.


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

* Plugin created by:
*   Dusan Davidovic
*   DusanDavidovic@gmail.com

 objecttype = 'TYPE'.  "Type pool


endmethod.
ENDCLASS.
