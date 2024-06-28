!problem =>   write a fortran program to output along with the following input data, the charges for use of electricity 
!of customers. An electricity company has three categories of customers : Industrial, Bulk Industrial and domestic.
!the rate are : a) for industrial minimum upto 150 units tk. X fixed. Next 150 units rate tk. P per unit. Next 
!200 unit rate tk. Q per unit and above this units rate tk. R per unit b)for bulk industrial minimum upto 100 units tk.
!y fixed. Next 100 units rate tk. U per unit. Next 200 unit rate tk. V per unit and above this units rate tk. W per unit 
!c)for Domestic minimum upto 50 units tk. A per unit minimum upto 75 units rate tk. B per unit. Next 
!125 units rate tk. C per unit. Next 200 units rate tk. D per unit and above this units rate tk. G per unit.

!Given the customer id number, category of customer, the previous meter reading and the current reading,
!demand charge tk. 60 then including vat 5%


program electricity_charges
    implicit none
    ! Constants for VAT and demand charge
    real, parameter :: vat_rate = 0.05
    real, parameter :: demand_charge = 60.0

    ! Rates for Industrial customers
    real, parameter ::  = 150.0, IND_P = 2.0, IND_Q = 3.0, IND_R = 4.0
    ! Rates for Bulk Industrial customers
    real, parameter :: BULK_FIXED = 100.0, BULK_U = 1.5, BULK_V = 2.5, BULK_W = 3.5
    ! Rates for Domestic customers
    real, parameter :: DOM_A = 1.0, DOM_B = 1.5, DOM_C = 2.0, DOM_D = 2.5, DOM_G = 3.0

    ! Variables
    integer :: customer_id, previous_reading, current_reading, units_used
    real :: charge, total_charge
    character(20) :: category

    ! Input data
    print *, 'Enter customer ID:'
    read *, customer_id
    print *, 'Enter category (Industrial, BulkIndustrial, Domestic):'
    read *, category
    print *, 'Enter previous meter reading:'
    read *, previous_reading
    print *, 'Enter current meter reading:'
    read *, current_reading

    !units used
    units_used = current_reading - previous_reading

    ! Calculate charge based on category
    select case (category)
    case ('Industrial')
        charge = calculate_industrial_charge(units_used)
    case ('BulkIndustrial')
        charge = calculate_bulk_industrial_charge(units_used)
    case ('Domestic')
        charge = calculate_domestic_charge(units_used)
    case default
        print *, 'Invalid category.'
        stop
    end select

    !demand charge and VAT
    total_charge = charge + DEMAND_CHARGE
    total_charge = total_charge * (1.0 + VAT_RATE)

    ! Output the results
    print *, 'Customer ID: ', customer_id
    print *, 'Category: ', category
    print *, 'Previous Meter Reading: ', previous_reading
    print *, 'Current Meter Reading: ', current_reading
    print *, 'Units Used: ', units_used
    print *, 'Total Charge: ', total_charge

contains

    real function calculate_industrial_charge(units)
        implicit none
        integer, intent(in) :: units
        real :: charge
        charge = 0.0

        if (units <= 150) then
            charge = IND_FIXED
        else if (units <= 300) then
            charge = IND_FIXED + (units - 150) * IND_P
        else if (units <= 500) then
            charge = IND_FIXED + 150 * IND_P + (units - 300) * IND_Q
        else
            charge = IND_FIXED + 150 * IND_P + 200 * IND_Q + (units - 500) * IND_R
        end if

        calculate_industrial_charge = charge
    end function calculate_industrial_charge

    real function calculate_bulk_industrial_charge(units)
        implicit none
        integer, intent(in) :: units
        real :: charge
        charge = 0.0

        if (units <= 100) then
            charge = BULK_FIXED
        else if (units <= 200) then
            charge = BULK_FIXED + (units - 100) * BULK_U
        else if (units <= 400) then
            charge = BULK_FIXED + 100 * BULK_U + (units - 200) * BULK_V
        else
            charge = BULK_FIXED + 100 * BULK_U + 200 * BULK_V + (units - 400) * BULK_W
        end if

        calculate_bulk_industrial_charge = charge
    end function calculate_bulk_industrial_charge

    real function calculate_domestic_charge(units)
        implicit none
        integer, intent(in) :: units
        real :: charge
        charge = 0.0

        if (units <= 50) then
            charge = units * DOM_A
        else if (units <= 125) then
            charge = 50 * DOM_A + (units - 50) * DOM_B
        else if (units <= 250) then
            charge = 50 * DOM_A + 75 * DOM_B + (units - 125) * DOM_C
        else if (units <= 450) then
            charge = 50 * DOM_A + 75 * DOM_B + 125 * DOM_C + (units - 250) * DOM_D
        else
            charge = 50 * DOM_A + 75 * DOM_B + 125 * DOM_C + 200 * DOM_D + (units - 450) * DOM_G
        end if

        calculate_domestic_charge = charge
    end function calculate_domestic_charge

end program electricity_charges



