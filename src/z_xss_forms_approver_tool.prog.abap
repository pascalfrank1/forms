*&---------------------------------------------------------------------*
*& Report z_xss_forms_approver_tool
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_xss_forms_approver_tool.

PARAMETERS: id      TYPE uuid,
            note    TYPE string LOWER CASE,
            approve RADIOBUTTON GROUP act DEFAULT 'X',
            reject  RADIOBUTTON GROUP act.


DATA(request) = zcl_form_request=>by_id( id ).

IF approve = 'X'.
  request->approve( note ).
ELSEIF reject = 'X'.
  request->reject( note ).
ENDIF.
