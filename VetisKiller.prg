// From Z:\Harbour\hb32\tests\ipsvr.prg 
#include "hbvo.ch"
#include "hbcurl.ch"

STATIC sCurlInitiated := .F.

	REQUEST HB_LANG_RU866		// Для HRB100 это и так работает
	REQUEST HB_LANG_RUWIN		// Для HRB300 нужна вся эта хрень,
	REQUEST HB_CODEPAGE_RU866	// при этом HB_LEGACY_LEVEL есть 
	REQUEST HB_CODEPAGE_RU1251	// только для HBR100
	#ifdef WVT
		REQUEST HB_GT_WVT
		REQUEST HB_GT_WVG
	#endif

// Dummy functions
FUNCTION Help();	RETURN .T.
FUNCTION Del_reg();	RETURN .T.
FUNCTION Compile(c);	RETURN c
FUNCTION FRSmena();	RETURN .T.

// /log=	0	Ничего не выводим
//		1	Ошибки
//		2	Основной протокол
//		4	Трассировка важных сообщений
//		8	Трассировка всех сообщений
//		16	Сохранение запросов и ответов
//		32	Игнорировать ошибки в дате
//		64	Автозапуск гашения без меню

PROCEDURE Main()
        LOCAL i, j, k, c, s, oVetis, oError, IntCP, kTitle, oldLogging, iAction
	LOCAL oVDList, oSEList
	LOCAL l1, kVert
	LOCAL cVersion := "1.03"
	LOCAL aActions := {	"Проверить наличие непогашенных сертификатов",;
				"Погасить входящие сертификаты",;
				"Получить порцию актуальных записей журнала",;
				"Циклическая отправка всего на ToBEGuid"}

	BEGIN SEQUENCE WITH {|errobj| ErrMacro(errobj)}

		IF MaxRow() = 299;	hb_Run("mode con cols=80 lines=25");	ENDIF	// Установлен дурацкий буфер экрана по умолчанию 300 строк

		kVert := INT((Maxrow() - 21) / 2)
		IntCP := hb_cdpOs()
		hb_cdpSelect(IntCP)			// По умолчанию

		DefPub("pubLogging", 0)			// Режим логов (0 = Можно все), д.б. ЗДЕСЬ
							// 	1	Запрет Логов
							//	2	Запрет Диалогов
							//	4	Запрет клавы
							//	8	Запрет дублирования ошибки в LowMes

		DefPub("pubAds", 1)			// 1 - ADS для cSysCtl, 2 - ADS для прочих
		LogStd("Vetis started")
	        
		SET DELETED ON
		SET WRAP ON
		SET CONFIRM ON
		SET DATE BRITISH
		SET EPOCH TO 1980
		SET( _SET_EVENTMASK, INKEY_ALL) 
		mSetCursor( .T. )
	        
		IF !sCurlInitiated
			curl_global_init()
			sCurlInitiated := .T.
		ENDIF
	        
// Экранная херня
		SETCOLOR("W/B,B/W,,,B/W")
		CLEAR SCREEN
		s := MEMOREAD("title.txt")		
		l1 := INT((Maxcol()-LEN(TRIM(MEMOLINE(s, Maxcol(),1))))/2)
		kTitle := MLCount(s, Maxcol())
		FOR i=1 TO kTitle
			c := TRIM(MEMOLINE(s, Maxcol(), i))
			IF (j:=At("V.M", c)) > 0
				c := Left(c, j+2) + " " + cVersion + " от " + DToC(FDate("VetisKiller.exe"))
			ENDIF
			IF (j:=AT("2019-",c)) > 0;	c := STUFF(c, j+5, 4, STR(YEAR(DATE()),4));	ENDIF
			@ i + kVert, l1 SAY c
		NEXT

// Открытие сессии
		oVetis := Vetis{}
		IF !oVetis:OK .OR. !oVetis:Open();	BREAK "Unable to open session";	ENDIF
		oldLogging := M->pubLogging

		iAction := 2				// По умолчанию - гашение ВСД
		DO WHILE IsSet(oVetis:iLog, 64) .OR. (iAction := MENU_ONE(,, aActions, "Vetis: What to do")) # 0
			IF LastKey() = 27;	EXIT;	ENDIF
			M->pubLogging := oldLogging
			LowMes()
			DO CASE
				CASE iAction = 1	// Получить порцию сертификатов
					IF Empty(oVDList := oVetis:GetVDList(oVetis:nvdList,0,"INCOMING","CONFIRMED"))
						ErrMes("Ошибка получения списка ВСД")
					ELSE
						LowMes(NTrim(oVDList:total) + " непогашенных ВСД",,-5)
					ENDIF
				CASE iAction = 2	// Погасить порцию сертификатов
					M->pubLogging := _Or(M->pubLogging, 2)				// NO DIALOG
					LowMes(NTrim(oVetis:KillThemAll()) + " documents killed",,-4)	// Forever
				CASE iAction = 3	// Получить порцию актуальных записей журнала
					IF Empty(oSEList := oVetis:GetSEList(oVetis:nvdList,0))
						ErrMes("Ошибка получения списка stock entries")
					ELSE
						LowMes(NTrim(oSEList:total) + " stock entries",,-3)
					ENDIF
				CASE iAction = 4	// Циклическая отправка всего на ToBEGuid
					k := AskOne(1, "Число гашений",, "@KZ 9999")
					oVetis:kKilled := 0
					oVetis:kSent := 0
					M->pubLogging := _Or(M->pubLogging, 2)				// NO DIALOG
					DO WHILE oVetis:kKilled < k
						oVetis:KillThemAll(.F.)		// 1 цикл
						oVetis:SendThemAll()
						LowMes(NTrim(oVetis:kKilled) + " documents killed, " + ;
						       NTrim(oVetis:kSent) + " Stock entries sent",,-2)
						IF LastKey() = 27 .OR. Inkey(60) = 27;	EXIT;	ENDIF
					ENDDO
			ENDCASE
			IF IsSet(oVetis:iLog, 64);	EXIT;	ENDIF
		ENDDO

//
	        
	RECOVER USING oError
		YesErr("VetisMain", oError)
	END

	IF sCurlInitiated
		curl_global_cleanup()
		sCurlInitiated := .F.
	ENDIF

	IF !Empty(oVetis) .AND. oVetis:lOpened;	oVetis:Close();	ENDIF

	LogStd("Vetis stopped")
	
	RETURN

CLASS Vetis INHERIT HObject	// Vetis Session Class
/* DocumentType: Тип документа
	1	Товарно-транспортная накладная
	2	Конасамент
	3	CMR
	4	Авианакладная
	5	Транспортная накладная
	6	ТОРГ-12
	7	Ветеринарное разрешение на импорт продукции на территорию ТС
	8	Разрешение ветеринарного управления субъекта РФ на ввоз продукции на территорию субъекта.
	9	Акт отбора пробы на исследование
	10	ТОРГ-13
	11	Ветеринарный сертификат на перемещение внутри РФ
	12	Ветеринарный сертификат третьих стран на ввоз продукции на территорию РФ
	13	Ветеринарный сертификат страны ТС на ввоз продукции на территорию РФ
	14	Ветеринарный сертификат РФ на вывоз продукции на территорию стран ТС
	15	Ветеринарный сертификат РФ на вывоз продукции на территорию третьих стран
	16	Заказ
	17	Паспорт РФ
	18	Паспорт иностранного гражданина
	19	Паспорт гражданина Республики Казахстан
	20	Паспорт гражданина Республики Беларусь
	21	Паспорт гражданина Республики Армения
	22	Паспорт гражданина Республики Киргизия
	23	Универсальный передаточный документ (УПД)
	24	Электронный ветеринарный производственный сертификат
*/
/* PackageLevelType	Тип, описывающий уровень упаковки
	1	Внутренний уровень
                  Уровень, при котором упаковка отсутствует, но тем не менее есть необходимость наносить маркировку.
                  Например, яйцо, шкуры, мясо, сыр. Явно указывается, что упаковка отсутствует.
	2	Потребительский уровень
                  Товар в упаковке для розничной торговли, маркированный штриховым кодом для сканирования на кассе.
	3	Промежуточный уровень
                  Уровень упаковки, если он существует, который находится между потребительским и торговым уровнем.
	4	Торговый уровень
                  Товар в упаковке, предназначенной для заказа, оплаты и доставки.
                  Это согласованный между ритейлером и изготовителем (или другим участником) уровень упаковки товара,
                  в котором товар заказывается, оплачивается и доставляется.
	5	Дополнительный уровень</xs:documentation>
                  Товар в упаковке, которую нельзя однозначно отнести к торговому или транспортному уровню.
	6	Транспортный (Логистический) уровень
                  Товар в упаковке, предназначенной для отгрузки покупателю (ритейлеру) при выполнении заказа.
*/
/* EnterpriseRole	Тип, описывающий роль пердприятия
	UNKNOWN		Роль не определена
	PRODUCER	Является производителем продукции (в том числе выращивание)
	SLAUGHTER_HOUSE	Бойня (мясокомбинат)
	CUTTING_PLANT	Разделочное предприятие
	COLD_STORE	Холодильник
	PACKAGING_PLANT	Упаковочное предприятие
*/
/* ProductType	Тип, описывающий тип продукции
	1	Мясо и мясопродукты
	2	Корма и кормовые добавки	
	3	Живые животные
	4	Лекарственные средства
	5	Пищевые продукты
	6	Непищевые продукты и другое
	7	Рыба и морепродукты
	8	Продукция, не требующая разрешения
*/
/* ReferenceType	Тип отношения документа
	1	Сопроводительный документ. Complementary accompanying document
	2	Предшествующий документ. Preceding document (Ancestor)
	3	Следующий документ. Subsequent/following document (Child)
	4	Документ, взамен которого выдан текущий документ. Replaced document
	5	Документ, заменяющий текущий документ. Replaced by document
	6	Связанный документ. Related document
	7	Документ, подтверждающий происхождение партии продукции. Certificate of origin
*/
/* TransportationStorageType 	Способ хранения при перевозке
	FROZEN		Замороженный тип перевозки
	CHILLED		Охлажденный тип перевозки
	COOLED		Охлаждаемый тип перевозки
	VENTILATED	Вентилируемый тип перевозки
*/
/* TransportType	Тип транспорта
	1	Автомобильный
	2	Железнодорожный
	3	Авиатранспортный
	4	Морской (контейнер)	
	5	Морской (трюм)
	6	Речной (inland water)
	7	Перегон (скота)
*/
/* ResearchResult Результат лабораторного исследования/ВСЭ
	UNKNOWN		Результат неизвестен
	UNDEFINED	Результат невозможно определить (не нормируется)
	POSITIVE	Положительный результат
	NEGATIVE	Отрицательный результат
	UNFULFILLED	Не проводилось
	VSERAW		ВСЭ подвергнуто сырьё, из которого произведена продукция
	VSEFULL		Продукция подвергнута ВСЭ в полном объеме
*/
/* ProsperityType Статус благополучия
	UNDEFINED	Неопределенный (не определялся)
	UNKNOWN		Неидентифицированный/неизвестный (статус невозможно определить)
	SAFE		Благополучный
	UNSAFE		Неблагополучный
*/
/* DeliveryDecision Решение по поставке
	ACCEPT_ALL	Принять всю поставку
	PARTIALLY	Принять часть груза, на оставшуюся оформить возврат
	RETURN_ALL	Оформить возврат на всю поставку
*/
/* DeliveryInspectionResult Результат контроля поставки
	CORRESPONDS	Груз соответствует сведениям, заявленным в документах
	MISMATCH	Груз отличается от сведений, указанных в сопроводительных документах.
	UNSUPERVISED	Контроль не проводился
*/
/* VetDocumentForm Тип, описывающий форму ВСД
	CERTCU1		Форма 1 ветеринарного сертификата ТС
	LIC1		Форма 1 ветеринарного свидетельства
	CERTCU2		Форма 2 ветеринарного сертификата ТС
	LIC2		Форма 2 ветеринарного свидетельства
	CERTCU3		Форма 3 ветеринарного сертификата ТС
	LIC3		Форма 3 ветеринарного свидетельства
	NOTE4		Форма 4 ветеринарной справки
	CERT5I		Форма 5i ветеринарного сертификата
	CERT61		Форма 6.1 ветеринарного сертификата
	CERT62		Форма 6.2 ветеринарного сертификата
	CERT63		Форма 6.3 ветеринарного сертификата
	PRODUCTIVE	Форма производственного ветеринарного сертификата
*/
/* VetDocumentType Тип ветсертификата
	TRANSPORT	Транспортный
	PRODUCTIVE	Производственный
	RETURNABLE	Возвратный
	INCOMING	Входящий
	OUTGOING	Исходящий
*/
/* VetDocumentStatus  Статус ветсертификата
	CREATED		Создан. Неоформленный проект сертификата
	CONFIRMED	Оформлен. Действующий сертификат, по которому разрешено совершать транзакцию с грузом.
	WITHDRAWN	Аннулирован. Не действующий более сертификат
	UTILIZED	Погашен. Действующий сертификат, по которому транзакция уже была совершена.
	FINALIZED	Закрыт. Действующий производственный сертификат.
                  Статус проставляется при завершении производственной смены, изменение финализированного вет.сертификата не допускается.
                  Статус `FINALIZED` не используется в атрибуте `vetDStatus` сертификата, признак того, что производственный сертификат финализирован
                  определяется логическим атрибутом `finalized` сертификата. При этом сертификат всегда находится в статусе `CONFIRMED`.
*/
/* StockEntryBlankFilter
	ALL		Все записи журнала, вне зависимости от объёма
	BLANK		Записи журнала с нулевым объёмом
	NOT_BLANK	Записи журнала с ненулевым объёмом
*/
/* ProcessingProcedureType
	7	Замораживание
	12	Убой
	13	Упаковка/фасовка
	34	Нарезка/разделка
	35	Добыча (собирать, ловить, охотиться)
	37	Производство (из сырья вручную или с помощью машин)
	39	Обработка/переработка (посредством, как правило, рутинных процедур)
	40	Выращивание
	43	Хранение
	51	Сжигание (утилизация)
	73	Временное хранения
	95	Термическая обработка
	101	Утилизация (метод не определен или отличается от обозначенных выше вариантов)
	102	Сортировка и упаковка
*/




	CLASS VAR aSessions	INIT {}
	PROTECT niSys 		INIT 1			// Текущая версия программы
	PROTECT niRelease 	INIT 0			// Текущий релиз программы
	PROTECT handle		INIT 0			// Session handle
	PROTECT	aoDS		INIT {}			// oDS List
	PROTECT intCp
	EXPORT	lOpened		INIT .F.		// Сессия открыта
	PROTECT cLogFile 	INIT "errlog.txt"	// LOG
	PROTECT cPath		INIT ""			// ::cPath
	PROTECT aIni					//{ => } .INI file
	PROTECT selfName 	INIT "Vetis"
//	PROTECT saStdNs		INIT {}			// Common NameSpace array
	EXPORT aLast100	INIT {}			// Последние 100 погашенных сертификатов
	EXPORT aDelay		INIT {}			// Список отложенных сертификатов
	EXPORT aErr		INIT {}			// Список ошибочных сертификатов
	EXPORT kKilled		INIT 0			// Счетчик погашенных сертификатов
	EXPORT kSent		INIT 0			// Счетчик отправленных сртификатов

//================= Из .ini файла ===============================================
	EXPORT iLog 		INIT 3			// /log=	0	Ничего не выводим
							//		1	Ошибки
							//		2	Основной протокол
							//		4	Трассировка важных сообщений
							//		8	Трассировка всех сообщений
							//		16	Сохранение запросов и ответов
							//		32	Игнорировать ошибки в дате
							//		64	Автозапуск гашения без меню
	PROTECT scEndpoint				// EndPoint
	PROTECT scLogin					// VetisApi Login:pass 
	PROTECT scMercLogin				// Меркурий Login
	PROTECT scApiKey				// API key
	PROTECT scService				// Версия сервиса
	PROTECT scBEGuid				// GUID ХС (Business Entity)
	PROTECT scENGuid				// GUID предприятия (Enterprise)
	PROTECT cSysCtl					// Путь к БД
	EXPORT ToBEGuid					// BE, на который отправлять в Циклической отправке
	EXPORT ToENGuid					// EN, на который отправлять в Циклической отправке
	PROTECT Limit		INIT 40			// Суточный лимит гашений
	EXPORT dOffset		INIT 0			// Последний день гашения = Date() + ::dOffset

//================= /Из .ini файла ===============================================


//	PROTECT scTransID				// Номер последнего запроса
	PROTECT oCounter				// Counter
	PROTECT oVts					// Справочник сертификатов
	PROTECT oVtt					// Журнал транзакций
	PROTECT oVtd					// Словарь
	EXPORT iTimeOut		INIT 600		// TimeOut в сек. ожидания ответа не двойной запрос
	EXPORT iInterval 	INIT 10			// Интервал в сек. перезапроса ответа на двойной запрос
	EXPORT nvdList		INIT 1			// Число документов, выгребаемых за раз в GetVDList
	PROTECT aTTNTypes	INIT {1,2,3,4,5,6,10,23}	// Типы (DocumentType) a-la ТТН

	METHOD Close()						// Close session
	METHOD GetEnGuid()					// Get/Set Enterprise GUID
	METHOD GetResponse(cAppId, cEndPoint, iTimeOut, iInterval) 	// Get Response (двойной запрос)
	METHOD GetSEList(iCount, iOffset)			// Get StockEntries from Mercury
	METHOD GetVDList(iCount, iOffset, cType, cStatus)	// Get all new VSDs from Mercury
	METHOD KillThemAll(lForever)				// Пришибить все найденные сертификаты
	METHOD New()						// Создание: Без параметров, на случай OLE
	METHOD Open(pscLogin, pscMercLogin, pscApiKey, pscBEGuid, pscENGuid, pscEndpoint, pscService, pILog, pcSysCtl)	// Инициализация
	METHOD outLog(iFlag, cMsg, cAdd)			// Вывод сообщений
	METHOD PrepOutCons(oSE, kol, toBEGuid, toENGuid, dTtn, cnTtn, cnVehicle, cStorage)	// Подготовка отгрузки
	METHOD ProcIncCons(oVD, kolFact, oAct, lSilent)		// Приемка отгрузки
	METHOD SendThemAll()					// Послать все StockEntries в ::ToBEGuid/::ToENGuid (для организации дурацкого потока)
	METHOD SoapRequest(oXml, cEndPoint)			// Send SOAP request and get the answer
	METHOD Submit(oReq, cEndPoint, lDouble)			// Submit request (двойной запрос)
	METHOD XmlFromTemplate(cId, lDouble)			// Создать oXml из файла
	
ENDCLASS

// ========== Закрытие сессии ======================================
METHOD Close() CLASS Vetis		// Close session
	IF !Empty(::oCounter);	::oCounter:Close();	ENDIF
	IF !Empty(::oVts);	::oVts:Close();		ENDIF
	IF !Empty(::oVtt);	::oVtt:Close();		ENDIF
        IF !Empty(::oVtd);	::oVtd:Close();		ENDIF
	::outLog(2, "Session closed")
	::lOpened := .F.
	RETURN SELF

// ========== getEnGuid ===============================================
METHOD GetEnGuid() CLASS Vetis		// Get/Set Enterprise GUID
	LOCAL i, k, oError, oReq, oResp, oXml, cAppId, oList, oEn, oAddr, oBE, oKpp
	LOCAL cName, cInn, cEnGuid, cEnName, cEnKpp, cIni, cKpp, kCount, arAsk, aMenu:={}
	LOCAL cTag := "EnterpriseGuid="
	LOCAL cEndPoint := ::scEndPoint + "EnterpriseService"

	BEGIN SEQUENCE WITH {|oError| ErrMacro(oError)}
		IF Empty(oReq := ::XmlFromTemplate("getBusinessEntityByGuidRequest",.F.));	BREAK "!";	ENDIF
		oReq["...guid"]:Value := ::scBEGuid
		IF Empty(oResp := ::Submit(oReq,cEndPoint,.F.));	BREAK "Ошибка запроса ХС";	ENDIF
		oBE := oResp:Body:Response:businessEntity
		cName := hb_Utf8ToStr(oBE[":name"]:Value)
		cInn := oBE:inn:Value
		oAddr := oBE:juridicalAddress:Clone()		// Хуйня с адресом

		IF Empty(::scENGuid)			// Enterprise Guid не заполнен. Проверим, нет ли площадок
			IF Empty(oReq := ::XmlFromTemplate("getActivityLocationListRequest",.F.));	BREAK "!";	ENDIF
			oReq["...guid"]:Value := ::scBEGuid
			IF Empty(oResp := ::Submit(oReq,cEndPoint,.F.));	BREAK "Ошибка запроса списка предприятий";	ENDIF
			IF Empty(oList := oResp["...activityLocationList"]);	BREAK "Ошибка списка предприятий";		ENDIF	// Нет списка 
			k := 0
			IF (kCount := Val(oList:count)) > 0
				FOR i:=1 TO kCount
					oEn := NIL
					oEn := oList:location[i]:enterprise
					oKpp := oEn["Registration:kpp"]
					cKpp := ""
					IF !Empty(oKpp);	cKpp := oKpp:Value;	ENDIF
					AAdd(aMenu, {cKpp + " " + ;
							hb_utf8ToStr(oEn[":name"]:Value), oEn:guid:Value})
				NEXT
				AAdd(aMenu, {"/", ""})
				AAdd(aMenu, {"Создать новое предприятие", ""})
				IF (k := MENU_ONE(,, aMenu, "Выберите предприятие")) = 0;	BREAK;	ENDIF
				IF k = Len(aMenu);	k := 0;	ENDIF
			ENDIF
			IF k > 0			// Выбрано существующее предприятие
				cEnGuid := aMenu[k,2]
			ELSE				// Создать новое предприятие
				arAsk := Ask({	{"Название ", Trim(cName)+", площадка "+NTrim(kCount+1), "@S30"},;
						{"КПП      ", Space(11),				 "@S11"} }, ;
						"Реквизиты предприятия")
				IF LastKey() = 27;	BREAK;	ENDIF
				IF Empty(oReq := ::XmlFromTemplate("modifyEnterpriseRequest"));	BREAK "!";	ENDIF
				oEn := NIL
				oEn := oReq["...enterprise"]
				oEn[":name"]:Value		:= hb_StrToUtf8(Trim(arAsk[1]))	// Название Enterprise
				oEn:address:country:Replace(oAddr:country)	// Адрес - из ХС
				oEn:address:region:Replace(oAddr:region)
				IF !Empty(oAddr["district"]);	oEn:address:district:Replace(oAddr:district);	ENDIF
				IF !Empty(oAddr["locality"]);	oEn:address:locality:Replace(oAddr:locality);	ENDIF
				oEn:address:street:Replace(oAddr:street)
				oEn:address:enAddressView	:= oAddr:addressView:Value
				oEn:owner:guid			:= ::scBEGuid
				oEn:Registration:Entity:inn	:= cInn
				oEn:Registration:kpp		:= Trim(arAsk[2])	// КПП Enterprise

				IF Empty(cAppId := ::Submit(oReq));		BREAK "Ошибка создания предприятия";	ENDIF
				IF Empty(oResp := ::GetResponse(cAppId));	BREAK "Ошибка создания предприятия";	ENDIF
				cEnGuid := oResp["...enterprise\guid"]:Value
			ENDIF
			IF !Empty(cEnGuid)
				::scENGuid := cEnGuid
				cIni := hb_Memoread(::selfName + ".ini")	// Запись в vetis.ini
				IF cTag $ cIni
					cIni := StrTran(cIni, cTag, cTag + cEnGuid)
				ELSE
					IF !(Right(cIni,2) = CRLF);	cIni += CRLF;	ENDIF
					cIni += cTag + cEnGuid + CRLF
				ENDIF
				hb_MemoWrit(::selfName + ".ini", cIni)
			ELSE
				BREAK "Предприятие не определено"
			ENDIF
		ENDIF
		IF !Empty(::scENGuid)
			IF Empty(oReq := ::XmlFromTemplate("getEnterpriseByGuidRequest",.F.));	BREAK "!";	ENDIF
			oReq["...guid"]:Value := ::scENGuid
			IF Empty(oResp := ::Submit(oReq,cEndPoint,.F.));	BREAK "Ошибка запроса предприятия";	ENDIF
			cEnName := oResp["...enterprise"][":name"]:Value
			cEnKpp := ""
			IF !Empty(oKpp := oResp["...enterprise:Registration:kpp"])
				cEnKpp  := oKpp:Value
			ENDIF

			@ MaxRow() - 8, 0 Say Padc(cInn + " " + cName, MaxCol())
			@ MaxRow() - 7, 0 Say Padc(cEnKpp + " " + hb_utf8ToStr(cEnName), MaxCol())
			
		ENDIF

	RECOVER USING oError
		IF !Empty(oError)
			YesErr(::selfName + ":GetEnGuid", oError)
		ENDIF
	END
	RETURN ::scENGuid

// ========== VetisRequest ======================================
METHOD GetResponse(cAppId, cEndPoint, iTimeOut, iInterval) CLASS Vetis	// Get Response (двойной запрос)
	LOCAL oError, roXml, iCount:=0, cAdd
	LOCAL oSub, oApp, oAns, cStatus
	IF IsNil(iTimeOut);	iTimeOut := ::iTimeOut;		ENDIF
	IF IsNil(iInterval);	iInterval := ::iInterval;	ENDIF

	BEGIN SEQUENCE WITH {|oError| ErrMacro(oError)}

		oSub := oXML{,, ::cPath + "xml\Recieve.xml"}
		oApp := oSub:Body:Request
		oApp:apiKey			:= ::scApiKey
            	oApp:issuerId			:= ::scBEGuid
            	oApp:applicationId		:= cAppId

		DO WHILE .T.
			IF Empty(oAns := ::SoapRequest(oSub, cEndPoint));	BREAK "Ошибка получения ответа";	ENDIF
			IF !Empty(oAns:Body:Fault);				BREAK "Ошибка в запросе на ответ";	ENDIF
			cStatus := oAns:Body:Response:application:status:Value
			DO CASE
				CASE cStatus = "COMPLETED";	roXml := oAns;	EXIT
				CASE cStatus = "REJECTED";	roXml := NIL;	EXIT
				CASE cStatus = "IN_PROCESS"
				OTHERWISE;			BREAK "Странный ответ"
			ENDCASE
			IF iTimeOut = 0;			EXIT;	ENDIF	// TimeOut = 0: Ничего не ждать
			IF (iCount += iInterval) > iTimeOut;	EXIT;	ENDIF
			hb_idleSleep(iInterval)			// Просто спим
		ENDDO

	RECOVER USING oError
		IF !Empty(oError)
			cAdd := ""
			IF !Empty(oSub);	cAdd += "Request:" + CRLF + CRLF + oSub:ToXml() + CRLF + CRLF;	ENDIF
			IF !Empty(oAns);	cAdd += "Answer :" + CRLF + CRLF + oAns:ToXml() + CRLF + CRLF;	ENDIF
			IF !YesErr(::selfName + " GetResponse: ", oError, cAdd)
				roXml := NIL
			ENDIF
		ENDIF
	END
	RETURN roXml

// ========== getStockEntryListRequest ======================================
METHOD GetSEList(iCount, iOffset) CLASS Vetis		// Get StockEntries from Mercury
	LOCAL oError, oReq, oResp, oXml, cAppId

	BEGIN SEQUENCE WITH {|oError| ErrMacro(oError)}

		IF Empty(oReq := ::XmlFromTemplate("getStockEntryListRequest"));	BREAK "!";	ENDIF
		oReq:listOptions:count	:= NTrim(iCount)
		oReq:listOptions:offset	:= NTrim(iOffset)
		oReq:enterpriseGuid	:= ::scENGuid
		oReq:searchPattern:Delete()				// Пока непонятно

		IF Empty(cAppId := ::Submit(oReq));		BREAK "Ошибка запроса списка ВСД";	ENDIF
		IF Empty(oResp := ::GetResponse(cAppId));	BREAK "Ошибка получения списка ВСД";	ENDIF
		IF Empty(oXml := oResp["...StockEntryList"]);	BREAK;					ENDIF	// Нет списка документов

	RECOVER USING oError
		IF !YesErr(::selfName + ":GetSEList", oError);	oXml := NIL;	ENDIF
	END
	RETURN oXml

// ========== getVetDocumentListRequest ======================================
METHOD GetVDList(iCount, iOffset, cType, cStatus) CLASS Vetis		// Get all new VSDs from Mercury
	LOCAL oError, oReq, oResp, oXml, cAppId

	BEGIN SEQUENCE WITH {|oError| ErrMacro(oError)}

		IF Empty(oReq := ::XmlFromTemplate("getVetDocumentListRequest"));	BREAK "!";	ENDIF
		oReq:listOptions:count	:= NTrim(iCount)
		oReq:listOptions:offset	:= NTrim(iOffset)
		oReq:vetDocumentType	:= cType
		oReq:vetDocumentStatus	:= cStatus
		oReq:enterpriseGuid	:= ::scENGuid

		IF Empty(cAppId := ::Submit(oReq));		BREAK "Ошибка запроса списка ВСД";	ENDIF
		IF Empty(oResp := ::GetResponse(cAppId));	BREAK "Ошибка получения списка ВСД";	ENDIF
		IF Empty(oXml := oResp["...DocumentList"]);	BREAK;					ENDIF	// Нет списка документов

	RECOVER USING oError
		IF !YesErr(::selfName, oError);	oXml := NIL;	ENDIF
	END
	RETURN oXml

// ========== KillThemAll ======================================
METHOD KillThemAll(lForever) CLASS Vetis		// Пришибить все найденные сертификаты
	LOCAL i, kk, kTotal, kCount, oVDList, oVD, cVDUuid, cMsg, iOffset
	LOCAL iToday, tekDate, cWaiting, cLog4, LastKilled:="", cResult
	LOCAL aVd

	IF IsNil(lForever);	lForever := .T.;	ENDIF
	DO WHILE .T. .AND. NextKey() # 27
		LowMes(" ")
		tekDate := Date()
		IF (iToday := Val(DToS(tekDate))) # ::oCounter:Counter("TekDate",,0)	// Смена даты:
			::oCounter:Counter("TekDate",,0, iToday)			// Новая дата
			::oCounter:Counter("TekNumb",,0, 0)				// Сброс лимита
			::aDelay	:= {}						// Читаем с начала -- мб сутки сменились и ошибки ушли
//			::aErr	:= {}
		ENDIF

// Формируем в aVd очередную порцию необработанных, неошибочных и неотложенных ВСД
		kTotal	:= 0
		kCount	:= 0
		iOffset	:= 0
		aVd	:= {}
		DO WHILE !Empty(oVDList := ::GetVDList(::nvdList, iOffset, "INCOMING", "CONFIRMED"))
			kTotal	:= Val(oVDList:total)
			kCount	:= Val(oVDList:count)
			FOR i:=1 TO kCount
				oVd := oVDList:Document[i]
				cVDUuid := oVd:uuid:Value	// VD мог не успеть поменять свое состояние
				IF AScan(::aLast100, cVDUuid) = 0 .AND. ;		// Нет в последних 100 погашенных,
				   AScan(::aDelay  , cVDUuid) = 0 .AND. ;		// Нет в отложенных,
				   AScan(::aErr    , cVDUuid) = 0			// Нет в ошибочных
					AAdd(aVd, oVd:Clone())
					IF Len(aVd) >= ::nvdList;	EXIT;	ENDIF
				ENDIF
			NEXT
			iOffset += ::nvdList
			IF Len(aVd) >= ::nvdList;	EXIT;	ENDIF
			IF iOffset >= kTotal;		EXIT;	ENDIF
		ENDDO

// Обработка очередной порции
		cMsg := Time() + " GetVDList: " + NTrim(Len(aVd)) + " new documents"
		LowMes(cMsg,, -5)
		::outLog(4, cMsg)
		kk := 0						// Счетчик ВСД, погашенных в данной порции
		cWaiting := ""

		FOR i:=1 TO Len(aVd)
			IF NextKey() = 27;	EXIT;	ENDIF
			oVd := aVd[i]				// Очередной ВСД

			IF ::Limit > 0 .AND. ::oCounter:Counter("TekNumb",,0) >= ::Limit
				cWaiting := "На " + DToC(tekDate) + " лимит бесплатной версии (" + NTrim(::Limit) + ;
					" документов) исчерпан, осталось " + NTrim(kTotal - kk) + " документов"
				EXIT
			ENDIF

			cVDUuid := oVD:uuid:Value		// VD мог не успеть поменять свое состояние
			cLog4 := "VSD " + cVDUuid + ": "
			LowMes(cLog4,, -4)
			::outLog(16, cLog4, oVD:ToXml())

			cResult := NIL				// Очистка перед := : В cResult мб oXml, а в результате
			cResult := ::ProcIncCons(oVD,,,.T.)	// ProcIncCons - текстовая строка. Чтобы в :Value не лезло
			DO CASE
				CASE !Empty(cResult) .AND. ValType(cResult) = "C"	// Отложенный
					AAdd(::aDelay, cVDUuid)
					LowMes(cLog4 + " отложен",, -4)
					::outLog(4, cLog4 + cResult)
				CASE Empty(cResult) .OR. ValType(cResult) # "O"		// Ошибочный
					AAdd(::aErr, cVDUuid)
					LowMes(cLog4 + " с ошибкой",, -4)
					::outLog(1, cLog4 + "with error", oVD:ToXml())
				CASE ValType(cResult) = "O"
					AAdd(::aLast100, cVDUuid)
					IF Len(::aLast100) > 100;	hb_ADel(::aLast100, 1, .T.);	ENDIF
					::kKilled++
					kk++
					LowMes(cLog4 + " OK",, -4)
					::outLog(4, cLog4 + NTrim(::kKilled) + " OK")
					::outLog(16, cLog4, cResult:ToXml())
					LastKilled := "Последнее гашение " + DToC(DATE()) + " " + Time()
					cMsg := "KillThemAll: " + NTrim(::kKilled) + " documents killed, " + ;
								  NTrim(kTotal - kk) + " left, " + ;
								  NTrim(Len(::aErr)) + " errors, " + ;
								  NTrim(Len(::aDelay)) + " delayed"
					::oCounter:Counter("TekNumb")
					LowMes(Time() + " " + cMsg,,-1)
			ENDCASE
		NEXT
		IF Empty(cWaiting)
			cWaiting := Time() + " Ждем...     " + LastKilled
		ENDIF
		LowMes(cWaiting)
		IF LastKey() = 27;			EXIT;	ENDIF
		IF InKey(120) = 27;			EXIT;	ENDIF	// Отдохнем после ::nvdList, чоб все успело дойти
		IF kCount = 0 .AND. InKey(600) = 27;	EXIT;	ENDIF	// Все закончилось: Еще отдохнем
		IF !lForever;				EXIT;	ENDIF
	ENDDO
	LowMes(" ")
	RETURN ::kKilled

// ========== Инициализация ======================================
METHOD New() CLASS Vetis	// Создание: Без параметров, на случай OLE
	AAdd(::aSessions, SELF)
	::handle := ALen(::aSessions)
	::cPath := hb_dirBase()
	::intCp := hb_cdpOs()
	hb_cdpSelect(::intCp)		// По умолчанию
	LogStd(::selfName + " V.M " + NTrim(::niSys) + "." + NTrim(::niRelease))	
	RETURN SELF

// ========== Инициализация ======================================
METHOD Open(pscLogin, pscMercLogin, pscApiKey, pscBEGuid, pscENGuid, pscEndpoint, pscService, pILog, pcSysCtl) CLASS Vetis	// Инициализация

	LOCAL j, cFile, oError
	LOCAL cXmlHeader := '<?xml version="1.0" encoding="UTF-8"?>'

	BEGIN SEQUENCE WITH {|oError| ErrMacro(oError)}

		IF ::lOpened;				BREAK;				ENDIF

// .INI File
		IF !File(cFile := ::selfName + ".ini");	BREAK cFile + " не найден";	ENDIF
		IF !Empty(::aIni := hb_iniRead(cFile)["MAIN"])
			::ILog		:= Val(hb_HGetDef(::aIni, "iLog", "7"))		// Уровень сообщений в Log
			::scEndpoint	:= hb_HGetDef(::aIni,	"Endpoint", "https://api2.vetrf.ru:8002/platform/services/2.1/ApplicationManagementService")	// Vetis EndPoint
			::scService	:= hb_HGetDef(::aIni,	"Service", "mercury-g2b.service:2.0")	// Vetis EndPoint
			::scLogin	:= hb_HGetDef(::aIni,	"ApiLogin")		// Меркурий login:pass
			::scMercLogin	:= hb_HGetDef(::aIni,	"MercLogin")		// Меркурий login:pass
			::scApiKey	:= hb_HGetDef(::aIni,	"apikey")		// Vetis apikey
			::scBEGuid	:= hb_HGetDef(::aIni,	"BusinessEntityGuid")	// Меркурий GUID
			::scENGuid	:= hb_HGetDef(::aIni,	"EnterpriseGuid")	// Меркурий GUID
			::cSysCtl	:= hb_HGetDef(::aIni,	"DataPath", "")		// Путь к данным
			::nvdList	:= Val(hb_HGetDef(::aIni, "NVDList", "30"))	// Число документов, погашаемых за раз
			::ToBEGuid	:= hb_HGetDef(::aIni,	"ToBEGuid", "")		// BE, на который отправлять в Циклической отправке
			::ToENGuid	:= hb_HGetDef(::aIni,	"ToENGuid", "")		// EN, на который отправлять в Циклической отправке
			cXmlHeader	:= hb_HGetDef(::aIni,	"XMLHeader", "")	// Путь к данным
			::Limit		:= Val(hb_HGetDef(::aIni, "Limit", "40"))	// Суточный лимит гашений
			::dOffset	:= Val(hb_HGetDef(::aIni, "dOffset", "0"))	// Последний день гашения = Date() + dOffset
		ENDIF
	        
		IF !IsNil(pscLogin);		::scLogin	:= pscLogin;		ENDIF
		IF !IsNil(pscMercLogin);	::scMercLogin	:= pscMercLogin;	ENDIF
		IF !IsNil(pscApiKey);		::scApiKey	:= pscApiKey;		ENDIF
		IF !IsNil(pscBEGuid);		::scBEGuid	:= pscBEGuid;		ENDIF
		IF !IsNil(pscENGuid);		::scENGuid	:= pscENGuid;;		ENDIF
		IF !IsNil(pscEndpoint);		::scEndpoint	:= pscEndpoint;		ENDIF
		IF !IsNil(pscService);		::scService	:= pscService;		ENDIF
		IF !IsNil(pILog);		::ILog		:= pILog;		ENDIF
		IF !IsNil(pcSysCtl);		::cSysCtl	:= pcSysCtl;		ENDIF
	        
		IF Empty(::scLogin);		BREAK "Не задан login:pass Vetis.api";	ENDIF
		IF Empty(::scMercLogin);	BREAK "Не задан login Меркурия";	ENDIF	// Меркурий login:pass
		IF Empty(::scApiKey);		BREAK "Не задан apikey";		ENDIF	// Vetis apikey
		IF Empty(::scBEGuid);		BREAK "Не задан guid ХС";		ENDIF	// Меркурий GUID
//		IF Empty(::scENGuid);		BREAK "Не задан guid предпрития";	ENDIF	// Меркурий GUID
//		IF Empty(::cSysCtl);		BREAK "Не задан путь к данным";		ENDIF

		IF (j:=Rat("/", ::scEndPoint)) > 0					// Обрезание хвоста EndPoint:
			::scEndPoint := Left(::scEndPoint, j)				// Он будет переменным
		ENDIF
	        
		IF !File(cFile := "curl-ca-bundle.crt");		BREAK cFile + " не найден";	ENDIF
		IF !File(cFile := ::cPath + "xml\NameSpaces.txt");	BREAK cFile + " не найден";	ENDIF
		IF !File(cFile := ::cPath + "xml\Submit.xml");		BREAK cFile + " не найден";	ENDIF
		IF !File(cFile := ::cPath + "xml\Recieve.xml");		BREAK cFile + " не найден";	ENDIF
	        
// oXML CLASS VARs
		oXml():aXmlNs := ANameSpaces(::cPath + "xml\NameSpaces.txt")
		oXml():cXmlHeader := Substr(Left(cXmlHeader, Len(cXmlHeader)-1),2)	// Без <>

// DataBase
		IF !Empty(::cSysCtl)				// Есть БД
			IF Right(::cSysCtl,1) # "\";	::cSysCtl += "\";	ENDIF
			SetAds()
			::oCounter := Counter{::cSysCtl + "_vdCnt.dbf"}
			cFile := ::cSysCtl + "_vts.dbf"		// Справочник сертификатов
			IF !dbfCreate(cFile,,,1) .OR. !(::oVts := MyServer{cfile,"id"}):OK;	BREAK "Ошибка открытия "+cFile;	ENDIF
			cFile := ::cSysCtl + "_vtt.dbf"		// Журнал транзакций
			IF !dbfCreate(cFile,,,1) .OR. !(::oVtt := MyServer{cfile,"id"}):OK;	BREAK "Ошибка открытия "+cFile;	ENDIF
			cFile := ::cSysCtl + "_vtd.dbf"		// Словарь
			IF !dbfCreate(cFile,,,1) .OR. !(::oVtd := MyServer{cfile,"id"}):OK;	BREAK "Ошибка открытия "+cFile;	ENDIF
		ELSE
			::oCounter := Counter{::cPath + "xml\_vdCnt.dbf"}
		ENDIF

// Enterprise
		::scENGuid := ::GetEnGuid()			// Проверка/получение EnterPrise GUID
		IF Empty(::scENGuid)				// Enterprise GUID пустой:
			BREAK "Не задан guid предприятия"
		ENDIF	// Меркурий GUID
		IF ::Limit = 0;	::Limit := 40;	ENDIF

		::lOpened := .T.

	RECOVER USING oError
		::lOpened := YesErr(::selfName, oError)
	END

	IF ::lOpened;	::outLog(2, "Session opened");	ENDIF

	RETURN ::lOpened

// ========== Log ======================================
METHOD outLog(iFlag, cMsg, cAdd) CLASS Vetis		// Вывод сообщений
	IF IsSet(iFlag, ::ILog)
		IF iFlag = 1 .OR. cMsg = "*"
			LogErr(cMsg, cAdd)
		ELSE
			LogStd(cMsg, cAdd)
		ENDIF
		IF IsSet(iFlag, 32)
			? cMsg
		ENDIF
	ENDIF
	RETURN .T.

// ========== prepareOutgoingConsignmentRequest ======================================
METHOD PrepOutCons(oSE, kol, toBEGuid, toENGuid, dTtn, cnTtn, cnVehicle, cStorage) CLASS Vetis		// Подготовка отгрузки
	LOCAL oError, oReq, oResp, oXml, cAppId, oDelivery, oForms, oAuth, cdTtn

	BEGIN SEQUENCE WITH {|oError| ErrMacro(oError)}
		IF Empty(oSE);		BREAK "Wrong StockEntry";		ENDIF
		IF Empty(kol);		kol := Val(oSE:batch:volume:Value);	ENDIF	// По умолчанию все
		IF Empty(cStorage);	cStorage := "VENTILATED";		ENDIF
		DO CASE
			CASE Empty(dTtn);		dTtn := {DATE(), TIME()}
			CASE ValType(dTtn) = "A"
			CASE ValType(dTtn) = "D";	dTtn := {dTtn, TIME()}
		ENDCASE
		cdTtn := SQLDate(dTtn,, "T")

		IF Empty(oReq := ::XmlFromTemplate("prepareOutgoingConsignmentRequest"));	BREAK "!";	ENDIF

		oDelivery := oReq[":delivery"]			// oReq:delivery нельзя -- много ...delivery
		oDelivery:deliveryDate			:= cdTtn
		oDelivery:consignor:Entity:guid		:= ::scBEGuid
		oDelivery:consignor:enterprise:guid	:= ::scENGuid
		oDelivery:consignee:Entity:guid		:= toBEGuid
		oDelivery:consignee:enterprise:guid	:= toENGuid
		oDelivery:consignment:volume		:= kol
		oDelivery:consignment:unit:guid		:= oSE:batch:unit:guid:Value
		oDelivery:consignment:StockEntry:guid	:= oSE:guid:Value
		oDelivery:transportInfo:transportType	:= "1"				// Автомобильный
		oDelivery:transportInfo:transportNumber:vehicleNumber := cnVehicle
		oDelivery:StorageType			:= cStorage

		oForms := oDelivery:Forms
		oForms:waybill:Number			:= cnTtn
		oForms:waybill:Date			:= Left(cdTtn,10)		// Время не допускается почему-то
		oForms:waybill:type			:= "1"				// ТТН

		oAuth := oForms:Certificate:authentication
		oAuth:purpose:guid			:= "0778b8cb-f49d-4ed9-88b9-5f70af00a211"	// Реализация в пищу людям
		oAuth:Inspected				:= "false"
		oAuth:Expertized			:= "UNFULFILLED"
//		oAuth:Prosperity			:= "Благополучна"

		IF Empty(cAppId := ::Submit(oReq));		BREAK "Ошибка запроса отправки ВСД";	ENDIF
		IF Empty(oResp := ::GetResponse(cAppId));	BREAK "Ошибка результата отправки ВСД";	ENDIF
		IF Empty(oXml := oResp);			BREAK;					ENDIF	// Нет списка документов

	RECOVER USING oError
		IF !YesErr(::selfName + ":PrepOutCons", oError);	oXml := NIL;	ENDIF
	END
	RETURN oXml

// ========== processIncomingDeliveryRequest ======================================
METHOD ProcIncCons(oVD, kolFact, oAct, lSilent) CLASS Vetis	// Приемка отгрузки
								// oVD		Сертификат
								// kolFact 	Фактически принимаемое количество (NIL=Все)
								// oAct		Акт расхождений (при их наличии)
	LOCAL i, k, a, oError, oReq, oResp, oXml, cAppId, oVDCons, oDelivery, oFacts, oRet, kolVD, oVolume, cDecision
	LOCAL oWayBill, oForms, cRelDocName, oRefDoc, oRelDoc, cdTtn, oPoint, oVehNumb, cVehNumb, dTtn, tTtn, oDExpire
	LOCAL dExpire, oldPubLog, cYY, cMM, cDD, iDD

	IF IsNil(lSilent);	lSilent := .F.;	ENDIF
	IF lSilent
		oldPubLog := M->pubLogging
		M->pubLogging := _Or(M->pubLogging, 2 + 4)		// Запрет экрана и клавы
	ENDIF
	BEGIN SEQUENCE WITH {|oError| ErrMacro(oError)}

		IF Empty(oReq := ::XmlFromTemplate("processIncomingConsignmentRequest"));	BREAK "!";	ENDIF
		IF Empty(oVDCons := oVD["...certifiedConsignment"]);	BREAK "Wrong VD structure";	ENDIF
		IF Empty(oVolume := oVDCons["batch\volume"]);		BREAK "No volume in VD";	ENDIF
		IF (kolVD := Val(oVolume:Value)) = 0;			BREAK "Zero volume in VD";	ENDIF
		IF IsNil(kolFact);	kolFact := kolVd;	ENDIF
		kolFact := Min(kolFact, kolVd)
		DO CASE
			CASE kolFact = 0;				cDecision := "RETURN_ALL"
			CASE (Abs(kolFact - kolVd)/kolVd) < 0.05;	cDecision := "ACCEPT_ALL"
			OTHERWISE;					cDecision := "PARTIALLY"
		ENDCASE

		oDelivery := oReq[":delivery"]			// oReq:delivery нельзя -- много имен ...delivery
		oDelivery:consignor:Replace(oVDCons:consignor)
		oDelivery:consignee:Replace(oVDCons:consignee)
		IF cDecision == "RETURN_ALL"
			oDelivery:consignment:Replace(NIL)
		ELSE
			oDelivery:consignment:Replace(oVDCons:batch)		// batch => consignment
			IF cDecision == "PARTIALLY"
				oDelivery:consignment:volume := NTrim(kolFact)
			ENDIF 
		ENDIF
		oDelivery:broker:Replace(oVDCons["broker"])			// Optional
//		oDelivery:shipmentRoute:Replace(oVDCons["shipmentRoute"])	// Optional
		oDelivery:transportInfo:Replace(oVDCons["transportInfo"])	// Optional
		oDelivery:StorageType:Replace(oVDCons["StorageType"])		// Optional
		IF !Empty(oPoint := oVDCons["...routePoint"])
			k := oPoint:Counter()
			FOR i:=1 TO k
				IF !Empty(oVehNumb := oPoint[i]["...vehicleNumber"])
					cVehNumb := oVehNumb:Value
				ENDIF
			NEXT
			IF !Empty(cVehNumb) .AND. !Empty(oVehNumb := oDelivery:transportInfo["...vehicleNumber"])
				oVehNumb:Value := cVehNumb
			ENDIF
		ENDIF

		oForms := oDelivery:Forms
		oForms:Certificate:uuid := oVD:uuid:Value

// Разбираемся с RelatedDocument/ReferencedDocument
		cRelDocName := oForms:relatedDocument:cXmlName			// Просто сохраняем имя из сжемы
		oForms:relatedDocument:Delete()					// Чтоб не мешалась - будем импортировать
		IF !Empty(oRefDoc := oVD["referencedDocument"])			// Их там может быть много
			k := oRefDoc:Counter()
			FOR i:=1 TO k
				oRelDoc := oForms:AddSon(oRefDoc[i])
				oRelDoc:cXmlName := cRelDocName
				IF AScan(::aTTNTypes, Val(oRelDoc[":type"]:Value)) > 0	// Это ТТН
					IF Empty(oWayBill)			// WayBill определем из списка ReferencedDocument
						oWayBill := oRelDoc:Clone()
						oWayBill:relationshipType:Delete()
					ENDIF
				ENDIF
			NEXT
		ENDIF

// Дата поставки и WayBill
		a := SQLDate(oVD:lastUpdateDate:Value,,"T")		// Дата и время ВСД
		dTtn := CToD(a[1])
		tTtn := a[2]

		IF !Empty(oWayBill)
			cdTtn := oWayBill:issueDate:Value
			dTtn := Max(CTOD(SQLDate(cdTtn,,"T")[1]), dTtn)	// Дата поставки = Max(дата WayBill, дата ВСД)
			DO CASE
				CASE dTtn < Date();	tTtn := "23:59:59"
				CASE dTtn = Date();	tTtn := Time()
				CASE dTtn > Date();	tTtn := "00:00:01"
			ENDCASE

			IF dTtn > Date() + ::dOffset;	BREAK "!FUTURE";	ENDIF	// Особый случай: Отложить, Не совсем ошибка
			IF !Empty(oDExpire := oVDCons["batch\expiryDate\firstDate"])
				IF Empty(cYY := oDExpire["year"]);	BREAK "wrong expire year";	ENDIF
				IF Empty(cMM := oDExpire["month"]);	BREAK "wrong expire month";	ENDIF
				IF Empty(cDD := oDExpire["day"])			// Нет дня: Последний день месяца
					IF (iDD := Val(cMM:Value) + 1) > 12;	iDD := 1;	ENDIF
					dExpire := STOD(Padl(cYY:Value, 4, "0") + Padl(NTrim(iDD), 2, "0") + "01") - 1
				ELSE
					dExpire := CTOD(SQLDate(cYY:Value + "-" + cMM:Value + "-" + cDD:Value)[1])
				ENDIF
				IF dTtn > dExpire .AND. !IsSet(::ILog,32);	BREAK "Cargo expired";		ENDIF
			ENDIF
		ENDIF
		oDelivery:deliveryDate := SQLDate({dTtn, tTtn},, "T")
		oForms:waybill:Replace(oWayBill)

// Разбираемся с Delivery Facts
		oFacts := oReq:deliveryFacts
		oFacts:docInspection:responsible:login := ::scMercLogin
		oFacts:docInspection:result := IIF(Empty(oAct), "CORRESPONDS", "MISMATCH")
		oFacts:vetInspection:responsible:login := ::scMercLogin
		oFacts:vetInspection:result := "UNSUPERVISED"			// CORRESPONDS/MISMATCH/UNSUPERVISED
		oFacts:decision := cDecision					// ACCEPT_ALL/RETURN_ALL/PARTIALLY

// Разбираемся с несоответствиями поставки -- если есть акт
		oReq:discrepancyReport:Replace(oAct)				// Акт несоответствия, если есть

// Разбираемся с возвратом - если фактическое количество # количеству в ВСД
		oRet := oReq:returnedDelivery
		IF cDecision == "ACCEPT_ALL"                                    // Принимаем все
			oRet:Replace(NIL)
		ELSE								// Оформляем возврат
			oRet := oRet:Replace(oDelivery)				// Копия oDelivery, разница - отдельно
			oRet:consignor:Replace(oVDCons:consignee)		// Поменялись местами
			oRet:consignee:Replace(oVDCons:consignor)		// Поменялись местами
			oRet:consignment:volume := NTrim(kolVd - kolFact)
			oRet:Forms:Certificate:AddSon(oVD:authentication)
		ENDIF

		IF Empty(cAppId := ::Submit(oReq));		BREAK "VSD killing request error";	ENDIF
		IF Empty(oResp := ::GetResponse(cAppId));	BREAK "VSD killing result error";	ENDIF
		IF Empty(oXml := oResp);			BREAK;					ENDIF	// Нет списка документов

	RECOVER USING oError
		IF ValType(oError) = "C" .AND. oError = "!FUTURE"
			oXml := oError
		ELSE
			IF !YesErr(::selfName + ":ProcIncCons", oError);	oXml := NIL;	ENDIF
		ENDIF
	END

	IF lSilent;	M->pubLogging := oldPubLog;	ENDIF
	RETURN oXml

// ========== SendThemAll ======================================
METHOD SendThemAll() CLASS Vetis	// Послать все StockEntries в ::ToBEGuid/::ToENGuid (для организации дурацкого потока)
	LOCAL i, k:=0, oSEList, oSE, iOffset:=0, cMsg, cMsg2

	DO WHILE .T.
		IF Empty(oSEList := ::GetSEList(::nvdList,iOffset));	EXIT;	ENDIF

		cMsg := Time() + " GetSEList " + oSEList:count + " Stock entries "
		LowMes(cMsg,, -3)
		::outLog(16, cMsg)
		FOR i:=1 TO Val(oSEList:count)
			oSE := oSEList:stockEntry[i]:Clone()
			IF Val(oSE:batch:volume:Value) > 0
				cMsg2 := "Stock Entry " + NTrim(i)
				LowMes(cMsg + cMsg2,, -3)
				::outLog(16, cMsg2, oSE:ToXml())
				IF !Empty(::PrepOutCons(oSE,, ::ToBEGuid, ::ToENGuid,, StrTran(Time(),":","")+NTrim(i), "U390KH99"))
					k++
					::kSent++
				ENDIF
			ENDIF
		NEXT
		iOffset += ::nvdList
		IF iOffset >= Val(oSEList:total);	EXIT;	ENDIF
	ENDDO
	cMsg := Time() + " SE sent: " + NTrim(k)
	LowMes(cMsg,, -3)
	::outLog(4, cMsg)
	RETURN k

// ========== SoapRequest ======================================
METHOD SoapRequest(oXml, cEndPoint) CLASS Vetis	// Send SOAP request and get the answer
	LOCAL j, roXml, cXml, cAction:="", cAns
	LOCAL curlHandle, curlErr
	LOCAL aHeader:={}

	IF IsNil(cEndPoint);	cEndPoint := ::scEndPoint + "ApplicationManagementService";	ENDIF
	IF !Empty(oXml)
		cXml := oXml:ToXML(.T.)			// AsRoot
		cAction := oXml:Body:Request:cXmlName	// ??? В SoapUI это ...\data\Request
		IF (j := At(":", cAction)) > 0;	cAction := Substr(cAction, j+1);	ENDIF
		cAction := AllTrim(cAction)
		::outLog(16,, "=== " + cAction + " to " + cEndPoint + " Request: " + CRLF + CRLF + cXml + CRLF)
		AADD(aHeader, "Accept-Encoding: gzip,deflate")
		AADD(aHeader, "Content-Type: text/xml;charset=UTF-8")
		AADD(aHeader, 'SOAPAction: "' + cAction + '"')
		AADD(aHeader, "Connection: Keep-Alive")

		IF !Empty(curlHandle := curl_easy_init())
			FOR j:=1 TO Len(aHeader)
				curl_easy_setopt(curlHandle,HB_CURLOPT_HTTPHEADER, aHeader[j])	// Specify the Header data
			NEXT
			curl_easy_setopt(curlHandle, HB_CURLOPT_URL, cEndpoint)	// Set the endpoint to send the POST to
			curl_easy_setopt( curlHandle, HB_CURLOPT_DOWNLOAD )	// Setup response data
//			curl_easy_setopt( curlHandle, HB_CURLOPT_DL_FILE_SETUP, "test_dl.bin" )
			curl_easy_setopt( curlHandle, HB_CURLOPT_DL_BUFF_SETUP )
//			l_easy_setopt( curlHandle, HB_CURLOPT_HTTPAUTH, HB_CURLAUTH_BASIC)
			curl_easy_setopt( curlHandle, HB_CURLOPT_USERPWD, ::scLogin)
			curl_easy_setopt(curlHandle, HB_CURLOPT_POST, 1)	// Specify the POST data
			curl_easy_setopt(curlHandle, HB_CURLOPT_POSTFIELDS, cXml)
			curl_easy_setopt(curlHandle, HB_CURLOPT_CAINFO, "curl-ca-bundle.crt")

//			IF !IsSet(::ILog,16) .OR. Yes("Отправлять?")
				IF Empty(curlErr := curl_easy_perform(curlHandle))		// Do everything
					cAns := curl_easy_dl_buff_get( curlHandle )		// Store response in variable
					IF !("Envelope>" $ cAns)
						::outLog(1, "curl: Strange answer " + cAns)
						cAns := NIL
					ENDIF
				ELSE
					::outLog(1, curl_easy_strerror(curlErr))
				ENDIF
//			ELSE
//				cAns := ""
//				IF File("TmpAnswer.xml");	cAns := hb_Memoread("TmpAnswer.xml");	ENDIF
//			ENDIF

			curl_easy_cleanup( curlHandle )				// Clean-up libcurl

		ELSE
			::outLog(1, "No curl handle")
		ENDIF
	ENDIF
	IF !Empty(cAns)
		roXml := oXml{,, cAns}
		::outLog(16,, "=== " + cAction + " Answer: " + CRLF + CRLF + roXml:ToXml() + CRLF)
	ENDIF
	RETURN roXml

// ========== Submit ======================================
METHOD Submit(oReq, cEndPoint, lDouble) CLASS Vetis	// Submit request oReq (lDouble: одинарный/двойной запрос)
	LOCAL oError, cAdd, cAppId 
	LOCAL oSub, oApp, oAns

	IF IsNil(lDouble);	lDouble := .T.;		ENDIF
	BEGIN SEQUENCE WITH {|oError| ErrMacro(oError)}

		IF !Empty(oReq["TransactionId"]);	oReq:TransactionId := "TR-" + NTrim(::oCounter:Counter("Transact"));	ENDIF
		IF !Empty(oReq["initiator"]);		oReq:initiator:login := ::scMercLogin;	ENDIF

		IF lDouble
			oSub := oXML{,, ::cPath + "xml\Submit.xml"}
			oApp := oSub:Body:Request
			oApp:apiKey			:= ::scApiKey
			oApp:application:serviceId	:= ::scService
	            	oApp:application:issuerId	:= ::scBEGuid
        	    	oApp:application:issueDate	:= SQLDate({Date(), Time()},,"T")
			oApp:application:data:AddSon(oReq)
		ELSE
			oSub := oReq
		ENDIF

		IF Empty(oAns := ::SoapRequest(oSub, cEndPoint));	BREAK "Пустой ответ";		ENDIF
		
		IF !Empty(oAns["Body\Fault"]);		BREAK "Ошибка в запросе";	ENDIF
		
		::outLog(16,, "Answer0: " + CRLF + CRLF + oAns:ToXml() + CRLF)

		IF lDouble				// Двойной запрос, ответ будет в GetResponse		
			oAns := oAns:Body:Response:application
			IF !(oAns:status:Value == "ACCEPTED");	BREAK "Запрос не принят";	ENDIF
	        
			cAppId := oAns:applicationId:Value
		ELSE					// Одинарный запрос, ответ сразу
			cAppId := oAns			// Это и есть ответ
		ENDIF

	RECOVER USING oError
		IF !Empty(oError)
			cAdd := ""
			IF !Empty(oSub);	cAdd += "Request:" + CRLF + CRLF + oSub:ToXml() + CRLF + CRLF;	ENDIF
			IF !Empty(oAns);	cAdd += "Answer :" + CRLF + CRLF + oAns:ToXml() + CRLF + CRLF;	ENDIF
			YesErr(::selfName + " Submit: ", oError, cAdd)
		ENDIF
	END
	RETURN cAppId

// ========== XmlFromTemplate ======================================
METHOD XmlFromTemplate(cId, lDouble) CLASS Vetis		// Создать oXml из файла, выдать весь/[только Request]
	LOCAL oError, cFile, oXml, oRes

	IF IsNil(lDouble);	lDouble := .T.;	ENDIF		// Выдать только oXml:Body:Request для двойного запроса
	BEGIN SEQUENCE WITH {|oError| ErrMacro(oError)}
		cFile := ::cPath + "xml\" + cId + ".xml"
		IF !File(cFile);				BREAK cFile + " не найден";	ENDIF
		IF Empty(oXml := oXML{,, cFile});		BREAK cFile + " не разобран";	ENDIF
		IF lDouble				
			oRes := oXml:Body:Request		// Будет двойной запрос: Выдать только Request из oXml
		ELSE
			oRes := oXml				// Будет одинарный запрос: Выдать весь файл
		ENDIF
		IF Empty(oRes);					BREAK cFile + " не разобран";	ENDIF
	RECOVER USING oError
		IF !YesErr(::selfName, oError);	oRes := NIL;	ENDIF
	END
	RETURN oRes

