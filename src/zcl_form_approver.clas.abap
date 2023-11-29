CLASS zcl_form_approver DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Returns name of the given employee from IT1</p>
    class-methods: name_of importing employee_number type pernr_d returning value(name) type string.

    METHODS: constructor IMPORTING employee TYPE pernr_d approval_step TYPE i,
      get_manager RETURNING VALUE(manager) TYPE pernr_d,
      get_managers_manager RETURNING VALUE(managers_manager) TYPE pernr_d,
      get_clerk RETURNING VALUE(clerk) TYPE pernr_d,

      get_next_approver RETURNING VALUE(approver) TYPE pernr_d,
      get_next_approver_name RETURNING VALUE(name) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: employee TYPE pernr_d,
          step     TYPE i,
          approver TYPE pernr_d. "wird zwischengespeichert für Namen
ENDCLASS.



CLASS zcl_form_approver IMPLEMENTATION.
  METHOD constructor.
    me->employee = employee.
    me->step = approval_step.
  ENDMETHOD.

  METHOD get_next_approver.
    IF step = -1.
      RETURN.
    ENDIF.

    approver = me->approver = SWITCH #( step

        WHEN 0 THEN get_manager( )
        WHEN 1 THEN get_managers_manager( )
        WHEN 2 THEN get_clerk( )
     ).

  ENDMETHOD.

  METHOD get_clerk.

  ENDMETHOD.

  METHOD get_manager.

    manager = cl_hcmfab_employee_api=>get_instance( )->get_employeenumber_of_manager( iv_employee_pernr = employee iv_application_id = '' ).

  ENDMETHOD.

  METHOD get_managers_manager.
    managers_manager = cl_hcmfab_employee_api=>get_instance( )->get_employeenumber_of_manager( iv_employee_pernr = get_manager( ) iv_application_id = '' ).
  ENDMETHOD.

  METHOD get_next_approver_name.
    IF me->approver IS INITIAL.
      get_next_approver( ).
    ENDIF.

    SELECT SINGLE ename FROM pa0001 WHERE pernr = @approver AND begda <= @sy-datum AND endda >= @sy-datum INTO @name.

  ENDMETHOD.

  METHOD name_of.
    SELECT SINGLE ename FROM pa0001 WHERE pernr = @employee_number AND begda <= @sy-datum AND endda >= @sy-datum INTO @name.
  ENDMETHOD.

ENDCLASS.
