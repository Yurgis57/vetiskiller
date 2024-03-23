// Общие функции логов и ошибок

#include "hbvo.ch"

/*---------- YesErr ------------------------------------------------------------09.08.17--------*/
FUNCTION YesErr(cPrefix, oError, cAdd)		// Выдача системной ошибки
						//	cPrefix	Префикс сообщения
						//	oError	объект ошибки или текстовая строка
						//	cAdd	Поясняющая информация для Log
	LOCAL r := .F.
	LOCAL errLong	:= ErrText(oError, .F.)
	LOCAL errShort	:= IIF(Empty(cPrefix), "Ошибка", cPrefix) + ":;" + ErrText(oError, .T.)
	LOCAL colr_err	:= DefPub("colr_err", "W+/R,R/W,,,R/W")

	IF IsNil(cAdd);	cAdd := "";	ENDIF

	DO CASE
		CASE EMPTY(oError);					r := .T.	// Не о чем спрашивать
		CASE VALTYPE(oError) = "C" .AND. oError = "!"				// Ошибка, но спрашивать не надо
		CASE NO_DIALOG			// Запрет диалогов
		CASE VALTYPE(oError) = "C"
			r := YES(errShort + ";Продолжать ?" ,,colr_err)	// Ошибка, спросим, продолжать ли
		CASE VALTYPE(oError) = "O"					// Системная ошибка
			IF YES(errShort + ";Нужны подробности ?",,colr_err);	r := ErrStandard(oError);	ENDIF
	ENDCASE
	IF !r .AND. !NO_LOG	// Нет запрета логирования
		LogErr(errShort, errLong + CRLF + cAdd + CRLF)
	ENDIF
	RETURN r

/*--------- ErrDefault ------------------------------------------------------02.08.19 ----------------*/
FUNCTION ErrDefault(oError)	// Можно ли насрать на ошибку?
	LOCAL r
	LOCAL aIgns := {EG_ZERODIV, EG_NUMOVERFLOW}	// , EG_NUMERR ?
	LOCAL aRetr := {EG_CREATE, EG_OPEN, EG_APPENDLOCK}

	DO CASE
		CASE !(ValType(oError) = "O")
		CASE AScan(aIgns, oError:genCode) > 0 .AND. oError:CanSubstitute		// Деление на 0 = 0
			r := 0
		CASE AScan(aRetr, oError:genCode) > 0 .AND. oError:canRetry			// Ошибка открытия в сети
			r := (oError:Tries < 10)
			NetErr(.t.)				
//		CASE AScan(aRetr, oError:genCode) > 0 .AND. oError:canDefault			// Ошибка открытия в сети
//			r := .F.
//			NetErr(.t.)
	ENDCASE
	RETURN r

/*--------- ErrGen ------------------------------------------------------02.08.19 ----------------*/
FUNCTION ErrGen(cMsg, Subsystem)			// Генерация Ошибки
	LOCAL oError := ErrorNew()
	oError:severity    := ES_ERROR
	oError:genCode     := EG_LIMIT
	oError:subSystem   := IIF(Empty(Subsystem), "ErrGen", Subsystem)
	oError:subCode     := 0
	oError:description := cMsg
	oError:canRetry    := .F.
	oError:canDefault  := .F.
	oError:fileName    := ""
	oError:osCode      := 0
	Eval( ErrorBlock(), oError )
//	return __errRT_SBASE( nSubCode, nCode, NIL, ::className + ":" + cMsg, 1, self )
	RETURN .T.

/*--------- ErrMacro ------------------------------------------------------02.08.19 ----------------*/
FUNCTION ErrMacro(oError)	// Щадящая функция обработки ошибок
	LOCAL r := ErrDefault(oError)
	LOCAL errLong := ErrText(oError), errShort := ErrText(oError, .T.)
	LOCAL cCmdLine := DefPub("cCmdLine", "")
	IF IsNil(r)
		LogErr(errShort, errLong)
		IF ValType(oError) = "O" .AND. Empty(oError:cargo)
			oError:cargo := errLong		// Чтобы получить трассировку из места возникновения
		ENDIF
		IF ValType(cCmdLine) = "C" .AND. "/ERRSYS" $ cCmdLine
			r := ErrStandard(oError)	// Чтобы получить полную информацию
		ELSE
			BREAK oError
		ENDIF
	ENDIF
	RETURN r

*+========+=================================================+=================+
*| ERRMES | Процедура выдачи сообщения об ошибке            | 12 марта   1989 |
*+--------+-------------------------------------------------+-----------------+
*| Параметры: txt - Текст сообщения. Строки отделяются ";"                    |
*|            txe - дополнительный текст выриантов ответа на ошибку через ";" |
*|            clr - цвет сообщения                                            |
*|            snd - звук                                                      |
*|            clear - чистить буфера: NIL: все;	0: ничего                     |
*|            left - NIL - по центру; не NIL - влево                          |
*|            ntxe - вариант ответа по умолчанию                              |
*| ERRMES = номеру выбранного вырианта ответа                                 |
*+============================================================================+
FUNCTION ERRMES(txt, txe, clr, snd, clear, left, ntxe)

// ERRMES Не завершается 241 или цифрами (серыми или любыми) или сканером, или Enter

	LOCAL arrems, arrdop, i, k, irow, icol, max_dop, r, s1
	LOCAL old_clr, old_row:=ROW(), old_col:=COL(), key
	LOCAL yesCD2, ss := SScr(), iMax

	LOCAL colr_err := DefPub("colr_err", "W+/R,R/W,,,R/W")
	LOCAL sys_sound := DefPub("sys_sound", .F.)
	LOCAL bat := DefPub("bat", .F.)
	LOCAL timer := DefPub("timer", AFILL(ARRAY(10),0))

	IF IsNil(snd);		snd:=.T.;		ENDIF
	IF IsNil(left);		left:=.F.;		ENDIF
	IF IsNil(clr);		clr:=colr_err;		ENDIF

	IF NO_DIALOG				// Запрет диалогов

		r := 0
	ELSE
	
		s1 := SECONDS()
//		old_prt:=SET(_SET_PRINTFILE, cWrkCtl+"errmlog.txt",.T.)
		yesCD2 := EMPTY(clr)		// На cd2 выводим ошибку, только если 
						// errmes с родным цветом.
		IF sys_sound.AND.snd
			TONE(300,1)
			TONE(499,5)
		  	TONE(700,5)
		ENDIF
	
		DO CASE
			CASE (clear=NIL .AND. !bat);	KBRD_CLR()
			CASE (clear=0 .OR. bat);	SAVE_KBRD(1)
		ENDCASE
		old_clr:=SETCOLOR(clr)
	
		iMax := 21	// Max число строк
		IF VALTYPE(txe) = "C";	iMax -=4;	ENDIF
	
		arrems := ARRTXT(StrTran(txt, CRLF, ";"), iMax)
	
		IF VALTYPE(txe)="C"      && Задано дополнительное сообщение
			arrdop=ARRTXT(txe)
			max_dop=ARRMXL(arrdop)         && MAX длина строки
			FOR i=1 TO LEN(arrdop)+4
				AADD(arrems,SPACE(max_dop+4))
			NEXT
		ELSE                     && Не задано дополнительное сообщение
			AADD(arrems,"HАЖМИТЕ ЛЮБУЮ КЛАВИШУ")
		ENDIF
		IF !left
			ARRCTR(arrems)
		ENDIF
		k := ARRMXL(arrems)

* Вывод сообщения

		irow=INT((20-LEN(arrems)-2)/2)
		icol=MAX(1,INT((MAXCOL()+1-k-2)/2)+1)
		@ irow,icol,irow+LEN(arrems)+1,icol+k+2 BOX "+=+|+=+| "
		LogErr(txt)
		FOR i=1 TO LEN(arrems)
			@ irow+i,icol+1 SAY arrems[i]
		NEXT
		IF IS_TILL
			IF !EMPTY(SUBSTR(M->tillOPC,6,1)) .AND. yesCD2
				IF CDOUT(VAL(SUBSTR(M->tillOPC,6,1)), -1)	// Если Порт открыт
					CDOUT(VAL(SUBSTR(M->tillOPC,6,1)), 6, "Ошибка")
				ENDIF
			ENDIF
			IF !EMPTY(SUBSTR(M->tillOPC,7,1)) .AND. yesCD2
				IF VALPOS(M->tillOPC,7) > 0
					IF CDOUT(VAL(SUBSTR(M->tillOPC,7,1)), -1)	// Если Порт открыт
						CDOUT(VAL(SUBSTR(M->tillOPC,7,1)), 6, "Ошибка", .F.)
					ELSE
						IF SUBSTR(M->tillOPC,7,1)="F"
							LogErr()
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDIF
		IF bat .AND. REST_KBRD()=0; Key_board(CHR(K_ESC)) ; ENDIF

* Что дальше ?

		IF VALTYPE(txe)="C"      && Задано дополнительное сообщение
			IF ntxe=NIL;ntxe:=1;ENDIF
			r := MENU_ONE(irow+LEN(arrems)-LEN(arrdop)-3, icol+INT((k-max_dop)/2), arrdop,,,ntxe)
		ELSE                    // Не задано дополнительное сообщение
			r := 0  	// Из ERRMES не выходят по сканеру и по цифре, и по Enter
				
			DO WHILE (key:=MYINKEY(0)) = K_ENTER .OR. Between(key, 48, 57)
				IF sys_sound.AND.snd	//Попищим - а то кассир ни фига не замечает
					TONE(300,1)
					TONE(499,5)
					TONE(700,5)
				ENDIF
			ENDDO
		ENDIF
	        
		IF sys_sound
			TONE(300,1)
			TONE(499,5)
			TONE(700,5)
		ENDIF
		IF !IsNil(clear) .OR. bat
			REST_KBRD()
		ENDIF
		SETCOLOR(old_clr)
	        
		SET PRINTER TO
//		SET(_SET_PRINTFILE, old_prt, .T.)
		RScr(ss)
		SETPOS(old_row,old_col)
		s1 := SECONDS() - s1
		IF s1 > 0			// Исключаем время обработки ошибки из timer
			timer[4] += s1
		ENDIF
	ENDIF

	RETURN r

*+=============+===================================================+==========+
*| ErrStandard | Стандартная Функция обработки ошибок              | 04.08.19 |
*+-------------+---------------------------------------------------+----------+
*| Параметры:                                                                 |
*|   Error - Ошибочный объект                                                 |
*| fErrMain=.T. - RETRY                                                       |
*|          .F. - выход                                                       |
*+============================================================================+
FUNCTION ErrStandard(oError)	// Стандартная Функция обработки ошибок

	LOCAL r := ErrDefault(oError)
	LOCAL errLong := ErrText(oError)
	LOCAL errShort := ErrText(oError, .T.)
	LOCAL nChoice
	LOCAL ErrorText := "ВНИМАНИЕ !;"
	LOCAL ErrorMenu := NIL
	LOCAL realErrMemo
	LOCAL tillOpc := DefPub("tillOpc", Space(20))

	IF IsNil(r)
		LogErr(errShort, errLong)
		IF oError:canRetry;	ErrorMenu := "Выйти из программы;Продолжить работу";	ENDIF
		realErrMemo := oError:cargo			// Для реальной трассировки на момент возникновения ошибки
		IF Empty(realErrMemo);	realErrMemo := errLong;	ENDIF
		nChoice := ERRMES(ErrorText + realErrMemo, ErrorMenu)
		DO CASE
			CASE nChoice == 2;	r := .T.	// Продолжить работу: RETRY
			OTHERWISE				// Выход
				ErrorLevel(1)
				IF VAL(SUBSTR(tillOpc,4,1)) > 0
					KBD_OFF()
				ENDIF
				QUIT
		ENDCASE
	ENDIF
	RETURN r

/*--------- ErrText -------------------------------------------------------02.08.19 ----------------*/
FUNCTION ErrText(oError, lShort, lCr)	// Функция выдачи расшифровки ошибки В МОМЕНТ ЕЕ СОВЕРШЕНИЯ в текстовую строку
	LOCAL i
	LOCAL err := ""
	IF IsNil(lShort);	lShort := .F.;	ENDIF
	IF IsNil(lCr);		lCr := .T.;	ENDIF		// Разделитель строк - CRLF (иначе ";")

	DO CASE
		CASE ValType(oError) = "C";				err := oError
		CASE ValType(oError) = "O" .AND. !Empty(oError:cargo);	err := oError:cargo
			IF lShort
				IF (i:=At("Description:", err)) > 0;	err := "System error: " + Substr(err, i+12);	ENDIF
				IF (i:=At(";", err)) > 0;		err := Left(err, i-1);		ENDIF
				IF (i:=At(CRLF, err)) > 0;		err := Left(err, i-1);		ENDIF
			ENDIF
		CASE ValType(oError) = "O" .AND. lShort;		err := "System error: " + oError:description
		CASE ValType(oError) = "O" .AND. !lShort;		err := "Сообщение"
			DO CASE
				CASE oError:severity == ES_WHOCARES;	err := "Информация "
				CASE oError:severity == ES_WARNING ;	err := "Предупреждение "
				CASE oError:severity == ES_ERROR   ;	err := "Ошибка "
			ENDCASE

			IF !Empty(oError:osCode);	err += ";Ошибка OS  : " + NTrim(oError:osCode);	ENDIF
			IF !Empty(oError:description);	err += ";Description: "+ oError:description;	ENDIF
			IF !Empty(oError:filename);	err += ";Filename   : "+ oError:filename;	ENDIF
			IF !Empty(oError:operation);	err += ";Operation  : "+ oError:operation;	ENDIF
			IF !Empty(oError:genCode);	err += " GenCode= "+ NTrim(oError:genCode);	ENDIF
			IF !Empty(oError:OsCode);	err += " OsCode=  "+ NTrim(oError:osCode);	ENDIF
			IF !Empty(oError:SubCode);	err += " SubCode= "+ NTrim(oError:subCode);	ENDIF
			IF !Empty(oError:SubSystem);	err += ";SubSystem  : "+ oError:SubSystem;	ENDIF
			IF !Empty(oError:tries);	err += " Tries      : "+ NTrim(oError:tries);	ENDIF
			IF SELECT() > 0;		err += ";Alias()    : "+ ALIAS()+STR(RECNO());	ENDIF
// Стек
			FOR i:=1 TO 10
				IF (!Empty(ProcName(i)))
					err += 	";" + PADR(TRIM(PROCNAME(i))+"("+LTRIM(STR(PROCLINE(i)))+")",25) + ;
						IIF ((!Empty(ProcName(i+10))),;
							PADR(TRIM(PROCNAME(i+10))+"("+LTRIM(STR(PROCLINE(i+10)))+")",25),;
							SPACE(30))
				ELSE
					EXIT
				ENDIF
			NEXT
			IF lCr;	err := StrTran(err, ";", CRLF);	ENDIF
	ENDCASE
	RETURN err
		
/*--------- FOpenEx---------------------------------------------------02.08.19 ----------------*/
FUNCTION FOpenEx(cFile, lAppend)	// Создает или открывает имеющийся файл cFile для дополнения, создает недостающие каталоги
	LOCAL j, h
	IF IsNil(lAppend);	lAppend := .T.;	ENDIF
	IF !File(cFile)
		IF cFile = ".\"; cFile := hb_dirBase() + Substr(cFile,3);	ENDIF
		IF (j := RAt("\", cFile)) > 0
			hb_dirBuild(Left(cFile, j-1))
		ENDIF							// Не работает
		hb_memowrit(cFile,"")
	ENDIF
	IF (h := FOPEN(cFile, FO_READWRITE + FO_SHARED)) = F_ERROR
		h := NIL
	ELSE
		IF lAppend;	FSEEK(h, 0, FS_END);	ENDIF
	ENDIF
	RETURN h

/*--------- LogErr ---------------------------------------------------02.08.19 ----------------*/
FUNCTION LogErr(cMsg, cAdd)	// Вывод сообщения об ошибке в Log и дополнительного в ...
	IF IsNil(cMsg);			cMsg := "";				ENDIF
	IF !(cMsg = "!!! Error !!!");	cMsg := "!!! Error !!!  " + cMsg;	ENDIF
	LogStd(cMsg, cAdd)
	RETURN .T.

/*--------- LogIt ------------------------------------------------02.08.19 ----------------*/
FUNCTION LogIt(cFile, cMsg)	// Вывод сообщения в cFile
	LOCAL h
	LOCAL cPref := DTOC(DATE()) +" " + TIME() + " "
	IF !IsNil(cMsg)
		IF !Empty(h := FOpenEx(cFile))
			FWRITE(h, cPref + cMsg + CRLF)
			FCLOSE(h)
		ENDIF
	ENDIF
	RETURN .T.

/*--------- Log ------------------------------------------------------02.08.19 ----------------*/
FUNCTION LogStd(cMsg, cAdd)	// Вывод обычного сообщения в Log и дополнительного в ...
	IF IsNil(cMsg);		cMsg := "";									ENDIF
	IF cMsg = "*";		cMsg := "!!! Error !!! " + Substr(cMsg,2);					ENDIF
	IF !Empty(cMsg);	LogIt(".\log\log.txt", cMsg);							ENDIF
	IF cMsg = "!!! Err";	LogIt(".\log\errlog.txt", cMsg);						ENDIF
	IF !Empty(cAdd);	LogIt(".\log\" + DToS(Date()) + ".log", cMsg + CRLF + cAdd + CRLF + CRLF);	ENDIF
	RETURN .T.

