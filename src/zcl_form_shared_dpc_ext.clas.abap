CLASS zcl_form_shared_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_form_shared_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS:
      approvers_get_entity REDEFINITION,
      approvers_get_entityset REDEFINITION,
      personneldataset_get_entity REDEFINITION,
      notes_get_entityset REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_form_shared_dpc_ext IMPLEMENTATION.
  METHOD approvers_get_entity.
    DATA(approver_type) = it_key_tab[ name = 'TYPE' ]-value.

    DATA(current_employee) = cl_hcmfab_employee_api=>get_instance( )->get_employeenumber_from_user( ).

    DATA(approval) = NEW zcl_form_approver( employee = current_employee approval_step = 0 ).

    DATA(approver) = SWITCH #( approver_type
        WHEN 'M' THEN approval->get_manager( )
        WHEN 'MM' THEN approval->get_managers_manager( )
        WHEN 'C' THEN approval->get_clerk( )
     ).

    er_entity = VALUE #(
       employee_number = approver
       type = approver_type
       name =  zcl_form_approver=>name_of( approver )
    ).


  ENDMETHOD.

  METHOD approvers_get_entityset.
    DATA(current_employee) = cl_hcmfab_employee_api=>get_instance( )->get_employeenumber_from_user( ).

    DATA(approval) = NEW zcl_form_approver( employee = current_employee approval_step = 0 ).

    DATA(manager) = approval->get_manager( ).
    DATA(manangers_manager) = approval->get_managers_manager( ).
    DATA(clerk) = approval->get_clerk( ).


    et_entityset = VALUE #(
    (
        employee_number = manager
        type = 'M'
        name =  zcl_form_approver=>name_of( manager )
    )
    (
        employee_number = manangers_manager
        type = 'MM'
        name =  zcl_form_approver=>name_of( manangers_manager )
    )
    (
        employee_number = clerk
        type = 'C'
        name =  zcl_form_approver=>name_of( clerk )
    )
    ).
  ENDMETHOD.

  METHOD personneldataset_get_entity.

    DATA(employee_api) = cl_hcmfab_employee_api=>get_instance( ).
    DATA(employee_number) = employee_api->get_employeenumber_from_user( ).


    SELECT SINGLE vorna, nachn FROM pa0002 WHERE pernr EQ @employee_number AND begda <= @sy-datum AND endda >= @sy-datum INTO ( @DATA(first_name), @DATA(last_name) ).

    er_entity = VALUE #(
        employee_number = employee_number
        first_name = first_name
        last_name = last_name
        phone_number = employee_api->get_address_from_user( )-tel1_numbr

    ).



  ENDMETHOD.

  METHOD notes_get_entityset.


    data(id) = mr_request_details->filter_select_options[ property = 'headerId' ]-select_options[ 1 ]-low.

    data(current_employee) = cl_hcmfab_employee_api=>get_instance( )->get_employeenumber_from_user( ).
    data(request) = zcl_form_request=>by_id( id = conv #( id )  ).
    data(notes) = value zform_note_table( ).

*    loop at requests into data(request).
*    append lines of request->notes( ) to notes.
*    endloop.

    et_entityset = value #(
        for note in request->notes( )
        (
            request_id = request->id( )
            approval_step = note-approval_step
            from = zcl_form_approver=>name_of( note-sender )
            timestamp = note-timestamp
            note = note-message
        )
    ).



  ENDMETHOD.

ENDCLASS.
