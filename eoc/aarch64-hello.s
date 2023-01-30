.global main

main:
    stp x30, x19, [sp, -16]!
    b start

conclusion:
    ldp x30, x19, [sp], 16
	mov x8, 93 	// sys_exit() is at index 93 in kernel functions table
	svc #0 		// generate kernel call sys_exit(123);

start:
    mov x0, 1
    str x0, [sp, -16]!
    ldr x0, [sp], 16 
    add x0, x0, 41
    b conclusion
