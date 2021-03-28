;Archivo:	Main_lab_6.s
;dispositivo:	PIC16F887
;Autor:		Dylan Ixcayau
;Compilador:	pic-as (v2.31), MPLABX V5.45
;
;Programa:	Temporizadores
;Hardware:	dos displays en el puerto C y un led en el puerto A
;
;Creado:	23 mar, 2021
;Ultima modificacion:  28 mar, 2021
    
PROCESSOR 16F887
#include <xc.inc>

; configuración word1
 CONFIG FOSC=INTRC_NOCLKOUT //Oscilador interno sin salidas
 CONFIG WDTE=OFF	    //WDT disabled (reinicio repetitivo del pic)
 CONFIG PWRTE=ON	    //PWRT enabled (espera de 72ms al iniciar
 CONFIG MCLRE=OFF	    //pin MCLR se utiliza como I/O
 CONFIG CP=OFF		    //sin protección de código
 CONFIG CPD=OFF		    //sin protección de datos
 
 CONFIG BOREN=OFF	    //sin reinicio cuando el voltaje baja de 4v
 CONFIG IESO=OFF	    //Reinicio sin cambio de reloj de interno a externo
 CONFIG FCMEN=OFF	    //Cambio de reloj externo a interno en caso de falla
 CONFIG LVP=ON		    //Programación en bajo voltaje permitida
 
;configuración word2
  CONFIG WRT=OFF	//Protección de autoescritura 
  CONFIG BOR4V=BOR40V	//Reinicio abajo de 4V 
  
   PSECT udata_bank0 ;common memory
    cont:	DS  2 ;1 byte apartado
    banderas:	DS  1
    nibble:	DS  2 ;2 byte apartado
    display_var:    DS	2 ;2 byte apartado
    
   PSECT udata_shr ;common memory
    w_temp:	DS  1;Variable para el w temporal 
    STATUS_TEMP:   DS  1;Variable para el STATUS temporal
    
   PSECT resVect, class=CODE, abs, delta=2
  ;----------------------vector reset------------------------
  ORG 00h	;posición 000h para el reset
  resetVec:
    PAGESEL main
    goto main
    
  PSECT code, delta=2, abs
ORG 100h	;posicion para el codigo
;------------------ TABLA -----------------------
Tabla:
    clrf    PCLATH
    bsf	    PCLATH, 0
    andlw   0x0F
    addwf   PCL
    retlw   00111111B;0
    retlw   00000110B;1
    retlw   01011011B;2
    retlw   01001111B;3
    retlw   01100110B;4
    retlw   01101101B;5
    retlw   01111101B;6
    retlw   00000111B;7
    retlw   01111111B;8
    retlw   01101111B;9
    retlw   01110111B;A
    retlw   01111100B;b
    retlw   00111001B;c
    retlw   01011110B;d
    retlw   01111001B;E
    retlw   01110001B;F

PSECT intVect, class=CODE, abs, delta=2

  ;----------------------interrupción reset------------------------
  ORG 04h	;posición 0004h para interr
  push:			    
    movf    w_temp	    ;Guardamos w en una variable temporal
    swapf   STATUS, W	    ;Sustraemos el valor de status a w sin tocar las interrupciones
    movwf   STATUS_TEMP	    ;Guardamos el status que acabamos de guardar en una variable temporal
    
  isr:
    btfsc   T0IF	    ;Si el timer0  levanta ninguna bandera de interrupcion
    call    TMR0_interrupt  ;Rutina de interrupcion del timer0
    
    btfsc   TMR1IF	    ;Si el timer1  levanta ninguna bandera de interrupcion
    call    TMR1_interrupt  ;Rutina de interrupcion del timer0
    
    btfsc   TMR2IF	    ;Si el timer1  levanta ninguna bandera de interrupcion
    call    TMR2_interrupt  ;Rutina de interrupcion del timer0
  pop:
    swapf   STATUS_TEMP, W  ;Recuperamos el valor del status original
    movwf   STATUS	    ;Regresamos el valor a Status
    swapf   w_temp, F	    ;Guardamos el valor sin tocar las banderas a F
    swapf   w_temp, W	    ;El valor normal lo dejamos en w
    retfie		    ;Salimos de las interrupciones
    
;---------SubrutinasInterrupción-----------    
TMR0_interrupt:		    ;interrupción del timer0
    banksel TMR0	    ;se llama al banco donde se ubica el registro TMR0
    movlw   255		    ;Se le agrega el valor 255 a w
    movwf   TMR0	    ;movemos la literal a TMR0 
    bcf	    T0IF	    ;Limpiamos la bandera de interrupcion del timer 0
    bcf	    STATUS, 0	    ;Dejo el STATUS 0 en un valor de 0
    clrf    PORTD	    ;Limpio el puerto D
    btfss   PORTA, 0	    ;revisamos la led ubicada en el puerto RA0
    goto    p4		    ;vamos a la rutina p4
    btfsc   banderas, 1	    ;Las banceras me ayudaran a hacer los saltos entre cada display
    goto    display0	    ;Si la variable vandera es 1 en la posicion 1 vamos a la rutina del display0
    btfsc   banderas, 2	    
    goto    display1
    movlw   00000001B	    ;le agregamos un valor a la variable banderas
    andlw   0x0F	    ;reducimos el byte a 4 bits
    movwf   banderas	    ;
siguientedisplay:
    RLF	    banderas, 1	    ;Me ayuda a cambiar la bandera e ir al siguiente display
    return
    
p4:			    ;rutina para apagar los displays
    bcf PORTD,0		    ;apagamos ambos displays
    bcf PORTD,1
    return
    
display0:		    
    movf    display_var, w  ;La variable display tiene el valor que necesito ya modificado para hexadecimal
    movwf   PORTC	    ;Despues de pasar display_var a w movemos w al puertoC
    bsf	    PORTD, 0	    ;seteamos el pin del puerto D para controlar que display se mostrara
    goto    siguientedisplay
    
display1:
    movf    display_var+1, w
    movwf   PORTC
    bsf	    PORTD, 1
    goto    siguientedisplay
    
    
TMR1_interrupt:		    ;Interrupción del timer 1
    movlw   0x85	    ;le volvemos a asignar sus valores al timer 1
    movwf   TMR1H
	
    movlw   0xEE
    movwf   TMR1L
    bcf	    TMR1IF	    ;limpiamos la bandera del timer 1
    incf    cont	    ;incrementamos la el contador que manejara los displays
    return

TMR2_interrupt:		    ;Interrupcion del timer 2
    clrf    TMR2	    ;Limpiamos el tmr 2
    bcf	    TMR2IF	    ;Limpiamos la bandera del timer 2
    incf    PORTA	    ;incrementamos la led para que parpadee al tiempo del TMR1
    return
;-----------configuracion----------------------------  
main:
    banksel ANSEL	;configurar como digital
    clrf    ANSEL	
    clrf    ANSELH
    
    banksel TRISA	;configurar como salida los puertos seleccionados
    bcf	    TRISA, 0
    
    clrf    TRISC
    
    bcf	    TRISD, 0
    bcf	    TRISD, 1
    
    banksel PORTA	;reiniciar los puertos
    clrf    PORTA
    clrf    PORTC
    clrf    PORTD
    call    config_reloj    ;Configuracion de reloj para darle un valor al oscilador
    call    config_tmr0	    ;Configuracion del timer 0
    call    config_tmr1	    ;configuracion del timer 1
    call    config_tmr2	    ;configuracion del timer 2
    call    config_IE	    ;Configuracion de las interrupciones
 
loop:
    movf    cont, w	    ;movemos el valor del contador a w
    movwf   cont+1	    ;movemos el valor de w a la segunda variable con el nombre cont
    call    separar_nibbles ;llamamos a la rutina para separar los nibbles para mostrarlo en los displays
    call    config_displays ;llamamos a la rutina para configurar los displays
    goto    loop	    ;retornamos el loop


separar_nibbles:
    movf    cont+1, w	    ;Var tiene el valor del contador
    andlw   0x0f	    ;Obtenemos los 4 bits menos significativos
    movwf   nibble	    ;los pasamos a nibble
    swapf   cont+1, w	    ;volteamos la variable var
    andlw   0x0f	    ;obtenemos los 4 bits mas significativos
    movwf   nibble+1	   ; Los pasamos a nibble+1
    return

config_displays:
    movf    nibble, w	    ;Movemos el valor de nibble a w
    call    Tabla	    ;Movemos w a la tabla
    movwf   display_var	    ;el valor de w preparado para el display lo madamos a la variable display_var
    movf    nibble+1, w	    ;
    call    Tabla
    movwf   display_var+1
    return
    
config_reloj:
    banksel OSCCON	;Banco OSCCON 
    bcf	    IRCF2	;OSCCON configuración bit2 IRCF
    bsf	    IRCF1	;OSCCON configuracuón bit1 IRCF
    bcf	    IRCF0	;OSCCON configuración bit0 IRCF
    bsf	    SCS		;reloj interno , 250kHz
    return

config_tmr0:
    banksel OPTION_REG	    ;Banco de registros asociadas al puerto A
    bcf	    T0CS	    ; reloj interno clock selection
    bcf	    PSA		    ;Prescaler 
    bsf	    PS2
    bsf	    PS1
    bsf	    PS0		   ;PS = 111 Tiempo en ejecutar , 256
    
    banksel TMR0
    movlw   255
    movwf   TMR0
    bcf	    T0IF	;0.9 ms
    return

config_tmr1:
    banksel T1CON 
    bcf	    TMR1GE  ;ponemos al timer en configuracion de siempre contando
    bcf	    T1CKPS1 ;Preescaler 1:2
    bsf	    T1CKPS0
    bcf	    T1OSCEN ;Reloj interno
    bcf	    TMR1CS   
    bsf	    TMR1ON  ;Encendemos el TMR1
    movlw   0x85    ;Le asignamos un valor correspondiente al timer1 para 1 segundo  
    movwf   TMR1H
    movlw   0xEE
    movwf   TMR1L
    bcf	    TMR1IF  ;limpiamos la bandera del timer1
    return

config_tmr2:
    banksel T2CON   ;llamamos al banco donde esta ubicado T2CON
    movlw   11111111B	;configuramos bit por bit el registro T2CON con lo que necesitamos
    movwf   T2CON	
    
    Banksel PR2	    ;llamamos al banco donde esta ubicado PR2
    movlw   61	    ;le asignamos un valor al timer 2 segun su formula
    movwf   PR2	    
    
    
    banksel T2CON   
    clrf    TMR2    ;limpiamos el timer 2
    bcf	    TMR2IF  ;limpiamos la bandera del timer 2
    return
    
config_IE:	    
    banksel PIE1    ;llamamos al banco donde esta ubicado PIE
    bsf	    TMR1IE  ;habilitamos las interrupciones correspondientes
    bsf	    TMR2IE
    bsf	    T0IE
    banksel T1CON
    bsf	    GIE	    ;Habilitar en general las interrupciones, Globales
    bsf	    PEIE    ;habilitamos las interrupciones de los perifericos
    bcf	    TMR1IF  
    bcf	    TMR2IF
    bcf	    T0IF	;Limpiamos bandera
    return
END