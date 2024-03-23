/* Based on original code posted on this URL:
	https://github.com/vszakats/harbour-core/issues/181#issue-121482480 */

#require "hbmxml"

#include "simpleio.ch"
#include "hbmxml.ch"
#include "hbvo.ch"

STATIC s_mxml_error_msg := ""
STATIC ILog := 7			// /log=	0	Ничего не выводим
					//		1	Ошибки
					//		2	Основной протокол
					//		4	Трассировка важных сообщений
					//		8	Трассировка всех сообщений
					//		16	Трассировка импортируемых строк
STATIC cLogFile := ""			// LOG

//-------------------------------------------------------------------------------------------
CLASS oXML INHERIT HObject
	CLASS VAR IDCounter	INIT 0		// Unique object ID
	CLASS VAR cXmlHeader	INIT NIL	//'?xml version="1.0" encoding="UTF-8"?'
	CLASS VAR aXmlNs	INIT {}		// {{NameSpace, Path}}
	CLASS VAR aTrans	INIT {	{"&", "&amp;"},;
					{"<", "&lt;"},;
					{">", "&gt;"},;
					{'"', "&quot;"},;
					{"'", "&apos;"}	}

// Any node
	EXPORT ID
	EXPORT oXmlParent			// oXML - Parent  (Top: NIL)
	EXPORT cXmlName		INIT ""		// Node name
	EXPORT aXmlElements	INIT {}		// Node elements ( {oXML, ...} )
	EXPORT hXmlAttributes	INIT { => }	// Node attributes ( { name => value, ... } )
	EXPORT hXmlUserVars	INIT { => }	// Additional (user's) names + values
	EXPORT cXmlValue	INIT NIL	// Sic!!! node "simple" value

	METHOD AddNameSpaces(aXmlNs)	 	// Add Standard Namespaces from ::aXmlNs to oXML
	METHOD AddSibling(oXml)			// Add oXML to ::Parent:aXmlElements
	METHOD AddSon(oXml, iPos)		// Add [new] oXML to ::aXmlElements[iPos]
	METHOD Assign(uValue)			// Replace, Delete or Assign value to SELF, 
	METHOD ChangeOrder(iPos)		// Change oXML position in ::Parent:aXMLElements to iPos
	METHOD Clone(oParent)			// Clone SELF and set oParent for copy
	METHOD Counter()			// Number of siblings with the same name
	METHOD Delete(lAll)			// Delete SELF from everywhere
	METHOD DelNameSpaces()		 	// Delete Standard Namespaces from oXML attributes
	METHOD DelParent()			// Delete Parent referene
	METHOD DelSons() INLINE ::Delete(.F.)	// Delete all child elements
	METHOD FindElement(cPath, lFirst)	// Find cPath in oXml tree
	METHOD FromXML(sXml, cElement)		// Filling oXML tree from sXml string using mxml
	METHOD GetNameInfo( cName ) 		// Get Name description {{Area, cArea from name, Name, FullName, Value}}
	METHOD GetSiblings()			// Get array of all siblings with the same name
	METHOD GetStdNs(cNs, cPath)		// Get Standard Name for current Namespace
	METHOD MergeNameSpaces(aNs)	 	// Add new Namespaces from aNs to Standard Namespaces CLASS VAR ::aXmlNs
	METHOD New(oParent, cName, sXml, sXmlNs)
	METHOD NodeToObj(oXML, pNode, hNS)	// XML element => oXML
	METHOD noIVarGet( cName )
	METHOD noIVarPut( cName, uValue )
	METHOD Parent(oXml) 		SETGET	// Access/Assign oXmlParent
	METHOD ParseName( cName )		// Node name parsing, {Area [EAU], Mmodifiers [V], Normalized name}
	METHOD Replace(oXml, lSaveName)		// Delete SELF and replace it with oXml
	METHOD ShowNode(pNode, cText)		// Show Node info
	METHOD Top()			SETGET	// Get root node
	METHOD ToString(iType)			// Presentation of oXml
	METHOD ToXML(aStdNs, sXml, iOff)	// Put oXML tree to XML
	METHOD Value(uValue)		SETGET	// Access/Assign cXmlValue

// IF ":=" is oberloaded, debugger causes RTE when attempts to inspect the object
	OPERATOR ":=" ARG uValue INLINE ::Assign(uValue)	// SELF := uValue/SELF:value := uValue,  new oblect ignores oXML tree
	METHOD OperatorIndex(index)	OPERATOR "[]"		// Get i-th sibling of SELF, starting from 1
	METHOD OperatorMinus(uValue)	OPERATOR "-"	// Same as :=, but new oblect replaces existing and implies itself into oXML tree
	METHOD OperatorPlus(uValue) 	OPERATOR "+"	// Add new son to SELF

	ERROR HANDLER onError( ... )
ENDCLASS

//-------------------------------------------------------------------------------------------
METHOD AddNameSpaces(aXmlNs) CLASS oXML 	// Add Standard Namespaces from ::aXmlNs to oXML
	LOCAL j
	IF IsNil(aXmlNs);	aXmlNs := ::aXmlNs;	ENDIF
	IF !Empty(aXmlns)
		IF ValType(::hXmlAttributes) # "H";	::hXmlAttributes := { => };	ENDIF
		FOR j:=1 TO Len(aXmlns)
			::hXmlAttributes["xmlns:" + aXmlns[j,1]] := aXmlns[j,2]
		NEXT
	ENDIF
	RETURN .T.

//-------------------------------------------------------------------------------------------
METHOD AddSibling(oXml) CLASS oXML	// Add oXML to ::Parent:aXmlElements
	IF !Empty(::Parent)
		oXml := ::Parent:AddSon(oXml)
	ELSE
		ErrGen("Cannot add sibling for root: " + ToString(oXml))
	ENDIF
	RETURN oXml

//-------------------------------------------------------------------------------------------
METHOD AddSon(oXml, iPos) CLASS oXML	// Add [new] oXML to ::aXmlElements[iPos]
	IF !Empty(oXml)
		IF ValType(oXml) = "C"
			oXml := oXml{SELF, oXml}
		ELSE
			oXml := oXml:Clone(SELF)	// oXml:Parent := SELF
		ENDIF
		oXml:ChangeOrder(iPos)
	ENDIF
	RETURN oXml

//-------------------------------------------------------------------------------------------
METHOD Assign(uValue) CLASS oXML	// Replace, Delete or Assign value to SELF, 
	LOCAL r := uValue
	DO CASE
		CASE !(ValType(uValue) $ "CNM")
		CASE Len(::aXmlElements) > 0;	ErrGen("Node has elements " + ::cXmlName)
		OTHERWISE;			::Value := uValue;	r := SELF	// Sic!!! To simplify assignments
// !!! Maybe this is a bad idea - to assign directly without :Value
	ENDCASE
	RETURN r

//-------------------------------------------------------------------------------------------
METHOD ChangeOrder(iPos) CLASS oXML	// Change oXML position in ::Parent:aXMLElements to iPos
	LOCAL j, a
	IF !IsNil(::oXmlParent) .AND. !IsNil(iPos) .AND. iPos > 0
		a := ::oXmlParent:aXmlElements
		IF (j:=hb_AScan(a, SELF,,, .T.)) > 0
			hb_ADel(a, j, .T.)
		ENDIF
		IF iPos <= Len(a)
			hb_AIns(a, iPos, SELF, .T.)
		ELSE
			AAdd(a, SELF)
		ENDIF
	ENDIF
	RETURN SELF

//-------------------------------------------------------------------------------------------
METHOD Clone(oParent) CLASS oXML	// Clone SELF and set oParent for copy
// __objGetValueList(SELF) doesn't work properly because it gets CLASS VARs and SETGETS too
        LOCAL j, oXml := __objClone(SELF)
	oXML:oXMLParent := NIL		// To prevent deletion from oXMLParent:aXMLElements in oXml:Parent := ...
	oXml:Parent := oParent
	oXml:id := ++::IDCounter
	oXml:aXmlElements := {}
	FOR j:=1 TO Len(::aXmlElements)
		IF ValType(::aXmlElements[j]) = "O";	::aXmlElements[j]:Clone(oXml);	ENDIF
	NEXT
	RETURN oXml

//-------------------------------------------------------------------------------------------
METHOD Counter() CLASS oXML	// Number of siblings with the same name
	RETURN Len(::GetSiblings())

//-------------------------------------------------------------------------------------------
METHOD Delete(lAll) CLASS oXML	// Delete SELF from everywhere
	IF IsNil(lAll);	lAll = .T.;	ENDIF
	DO WHILE !Empty(::aXmlElements);	::aXmlElements[1]:Delete();	ENDDO
	IF lAll
		::DelParent()
		::OK := .F.
	ENDIF
	RETURN NIL

//-------------------------------------------------------------------------------------------
METHOD DelNameSpaces() CLASS oXML 	// Delete Standard Namespaces from oXML attributes
	LOCAL j, aKeys
	
	IF ValType(::hXmlAttributes) = "H"
		aKeys := hb_HKeys(::hXmlAttributes)
		FOR j:=1 TO Len(aKeys)
			IF aKeys[j] = "xmlns:";	hb_HDel(::hXmlAttributes, aKeys[j]);	ENDIF
		NEXT
	ENDIF
	RETURN .T.

//-------------------------------------------------------------------------------------------
METHOD DelParent() CLASS oXML	// Delete Parent referene
	LOCAL j
	IF !IsNil(::oXmlParent)
		IF (j:=hb_AScan(::oXmlParent:aXmlElements, SELF,,, .T.)) > 0
			hb_ADel(::oXmlParent:aXmlElements, j, .T.)
		ENDIF
		::oXmlParent := NIL
	ENDIF
	RETURN SELF

//-------------------------------------------------------------------------------------------
METHOD FindElement(cPath, lFirst) CLASS oXML	// Find cPath in oXml tree
	LOCAL i, j, k, cName, r:=NIL, aFound, aNodes, cOrigPath, cNewPath
	IF IsNil(lFirst);	lFirst := .T.;	ENDIF	// Root call
	IF lFirst .AND. !Empty(cPath)
		IF " " $ cPath;	cPath := Strtran(cPath, " ", "");	ENDIF
//		cPath := StrTran(cPath, ":", "\")	// ":" can be part of name
		cPath := StrTran(cPath, "/", "\")
		cPath := StrTran(cPath, "...", "\...")
		DO WHILE "\\" $ cPath;		cPath := StrTran(cPath, "\\", "\");	ENDDO
		DO WHILE "...\" $ cPath;	cPath := StrTran(cPath, "...\", "...");	ENDDO
		IF Left(cPath,1) = "\";		cPath := Substr(cPath,2);		ENDIF
	ENDIF

	cOrigPath := cPath
	IF !Empty(cPath)
		IF (j := At("\", cPath)) > 0			// Not the last node
			cName := Left(cPath, j-1)
			cPath := Substr(cPath, j+1)
		ELSE
			cName := cPath				// Last node
			cPath := ""
		ENDIF

		aNodes := {cName}
		IF cName = "..."
			aNodes := {Substr(cName,4)}		// cName without ...
			FOR i:=1 TO Len(::aXmlElements)
				AAdd(aNodes, ::aXmlElements[i]:cXmlName)
			NEXT
		ENDIF
			
		FOR k:=1 TO Len(aNodes)
			cNewPath := cPath					// Rest of Path
			IF k > 1;	cNewPath := cOrigPath;	ENDIF		// cName = "..." and this is Not cName: Add ...cName to son's path
			IF !Empty(aFound := ::GetNameInfo("E_" + aNodes[k]))	// cName not found
				IF !Empty(cNewPath)
					FOR i:=1 TO Len(aFound[4])
						IF !Empty(r := aFound[4,i]:FindElement(cNewPath, .F.));	EXIT;	ENDIF
					NEXT
				ELSE
//					IF aNodes[k] == StrTran(cName,"...","");	r := aFound[4,1];	ENDIF
					IF k = 1;	r := aFound[4,1];	ENDIF
				ENDIF
			ENDIF
			IF !Empty(r);	EXIT;	ENDIF
		NEXT
	ENDIF
	RETURN r

//-------------------------------------------------------------------------------------------
METHOD FromXML(sXml, cElement) CLASS oXML	// Filling (mxml) oXML tree from sXml string starting from cElement
	LOCAL pRoot, cName

	s_mxml_error_msg := ""

	mxmlSetErrorCallback(@my_mxmlError())
	IF !Empty(pRoot := mxmlLoadString( , sXml, @s_type_cb()))	// Find root element
//		ShowNode(pRoot, "Loading:")
//		ShowNode(mxmlFindFirst(pRoot), "FirstElement")
		IF HB_ISSTRING( cElement )				//	Find specified element
			pRoot := mxmlFindElement( pRoot, pRoot, cElement,,, MXML_DESCEND )
//			ShowNode(pRoot, "FindElement:")
		ELSE							// 	Find first top-level element
			IF mxmlGetType( pRoot ) = MXML_ELEMENT
				IF (cName := mxmlGetElement(pRoot)) = "?"
					IF IsNil(::cXmlHeader);	::cXmlHeader := cName;	ENDIF
					IF !(::cXmlHeader == cName)
						ErrGen("Incompatible headers " + ::cXmlHeader + " / " + cName)
					ENDIF
					pRoot := mxmlFindFirst(pRoot)
				ENDIF
			ELSE
				pRoot := mxmlFindFirst(pRoot)
			ENDIF
		ENDIF
	ENDIF
	IF !Empty(pRoot)
		::NodeToObj(SELF, pRoot)
		mxmlDelete(pRoot)
	ENDIF
	RETURN s_mxml_error_msg

//-------------------------------------------------------------------------------------------
METHOD GetNameInfo( cName ) CLASS oXML			// Get name description {Area, originalName, fullName, value}
	LOCAL i, j, c, y, uValue
	LOCAL aFound
	LOCAL a:=::ParseName(cName)
	LOCAL cArea		  			// Area to search name
//	LOCAL cMods					// Modifiers
	
	cArea := Left(a[1],1)  				// Area to search name
//	cMods := Substr(a[1],2)				// Modifiers
	cName := a[2]					// Normalized name

	aFound := {}
	IF !Empty(cName)
		IF Empty(aFound) .AND. cArea $ "E*"	// Looking for all Elements with name = cName
			FOR i:=1 TO Len(::aXmlElements)
				c := ::aXmlElements[i]:cXmlName
				IF Upper(Right(c, Len(cName))) == Upper(cName)
					uValue := ::aXmlElements[i]
//					IF "V" $ cMods				// Need value not object
//						uValue := uValue:cXmlValue
//						IF IsNil(uValue);	uValue := "";	ENDIF
//					ENDIF
					AAdd(aFound, {"E", cName, c, uValue})
				ENDIF
			NEXT
		ENDIF
	        
		IF Empty(aFound) .AND. cArea $ "A*"	// Looking for Attributes with name = cName
			FOR EACH y IN ::hXmlAttributes
				c := y:__enumKey()
				IF Upper(Right(c, Len(cName))) == Upper(cName)
					AAdd(aFound, {"A", cName, c, y:__enumValue()})
				ENDIF
			NEXT
		ENDIF
	        
		IF Empty(aFound) .AND. cArea $ "U*"	// Looking for User's vars with name = cName
			FOR EACH y IN ::hXmlUserVars
				c := y:__enumKey()
				IF Upper(Right(c, Len(cName))) == Upper(cName)
					AAdd(aFound, {"U", cName, c, y:__enumValue()})
				ENDIF
			NEXT
		ENDIF
	        
		IF !Empty(aFound)
			IF Len(aFound) > 1
				IF !(aFound[1,1] == "E")		// !"E": No duplications
					ErrGen("Duplicated name " + aFound[1,2])
					aFound := {}
				ENDIF
				FOR i:=2 TO Len(aFound)			// "E": If there are many elements, Area and FullName must be EQUAL
					FOR j:=1 TO i-1
						IF !(aFound[i,1] == aFound[j,1]) .OR. !(aFound[i,3] == aFound[j,3])
							ErrGen("Ambiguous name " + aFound[j,2])
							aFound := {}
							EXIT
						ENDIF
					NEXT
					IF Empty(aFound);	EXIT;	ENDIF
				NEXT
//				IF !Empty(aFound) .AND. "V" $ cMods	// Need value: No duplications
//					ErrGen("Ambiguous value for " + aFound[1,2])
//					aFound := {}
//				ENDIF
			ENDIF
			IF !Empty(aFound) .AND. aFound[1,1] == "E"	// aFound => 1-dimension, aFound[1,4]:=Array of objects
				aFound[1,4] := {aFound[1,4]}
				FOR i:=2 TO Len(aFound)
					AAdd(aFound[1,4], aFound[i,4])
					hb_ADel(aFound, i, .T.)
				NEXT
			ENDIF
		ENDIF
		IF !Empty(aFound);	aFound := aFound[1];	ENDIF
	ENDIF
	RETURN aFound

//-------------------------------------------------------------------------------------------
METHOD GetSiblings() CLASS oXML	// Get array of all siblings with the same name
	LOCAL i, a:={}, aEl, cName := ::cXmlName
	IF !IsNil(::oXmlParent)
		aEl := ::oXmlParent:aXmlElements
		FOR i:=1 TO Len(aEl)
			IF aEl[i]:cXmlName == cName;	AAdd(a, aEl[i]);	ENDIF
		NEXT
	ELSE
		a := {SELF}
	ENDIF
	RETURN a

//-------------------------------------------------------------------------------------------
METHOD GetStdNs(cNs, cPath) CLASS oXML	// Get Standard Name for current Namespace
	LOCAL j, cNsNew := cNs
	LOCAL aBadNs := {"", "ns", "tns"}
	IF (j := AScan(::aXmlNs, {|x| x[2] == cPath})) = 0	// New NameSpace (no such path)
		IF AScan(aBadNs, {|x| x == cNsNew}) > 0
			cNsNew := "v0"
		ENDIF
		DO WHILE AScan(::aXmlNs, {|x| x[1] == cNsNew}) > 0
			j++
			cNsNew := "v" + NTrim(j)
		ENDDO
		AAdd(::aXmlNs, {cNsNew, cPath})			// Register new namespace
	ELSE
		cNsNew := ::aXmlNs[j,1]
	ENDIF
	RETURN cNsNew

//-------------------------------------------------------------------------------------------
METHOD MergeNameSpaces(aNs) CLASS oXML 	// Add new Namespaces from aNs to Standard Namespaces CLASS VAR ::aXmlNs
	LOCAL i, j
	FOR i:=1 TO Len(aNs)
		j := AScan(::aXmlNs, {|x| x[1] == aNs[i,1]})
		DO CASE
			CASE j = 0;			AAdd(::aXmlNs, {aNs[i,1], aNs[i,2]})
			CASE ::aXmlNs[j,2] == aNs[i,2]
			OTHERWISE;			ErrGen("Ambigous NameSpace " +  aNs[i,1])
		ENDCASE
	NEXT
	RETURN .T.

//-------------------------------------------------------------------------------------------
METHOD New(oParent, cName, sXml, sXmlNs) CLASS oXML
	LOCAL cErr, aNs
//	IF !IsNil(oParent) .AND. ValType(oParent) = "O";	::Parent := oParent;	ENDIF
	::Parent := oParent
	::ID := ++::IDCounter
	IF !IsNil(cName) .AND. !(":" $ cName)
		cName := ::ParseName(cName)[2]
		IF !(":" $ cName); ErrGen("No namespace in " + cName);	ENDIF
		::OK := .F.
	ENDIF
	::cXmlName 		:= cName

	IF IsNil(oParent)						// Root
//		::aXml := {SELF}
//		::aXmlNs := {}
//		::cXmlNs := sXmlNs
		IF !IsNil(sXmlNs)					// Standard NameSpaces: Array / fileName/ String / WSDL path
			IF ValType(sXmlNs) = "A"			// Std NS as array {{ns without ":", path}}
				aNs := sXmlNs				// Sic!!! Not AClone - it should be common for the whole application, as it can be added in process !!!
			ELSE						// Std NS as filename or string
				aNs := ANameSpaces(sXmlNs)
			ENDIF
			::MergeNameSpaces(aNs)				// Add new namespaces to CLASS VAR ::aXmlNs
		ENDIF
		IF !IsNil(sXml)						// XML source fileName/ XML String
			sXml := AllTrim(sXml)
			IF !(sXml = "<") .AND. File(sXml)
				sXml := AllTrim(hb_Memoread(sXml))
			ENDIF
			IF sXml = "<"
//				::cXmlString := sXml
				IF !Empty(cErr := ::FromXml(sXml));	ErrGen(cErr);	ENDIF
			ENDIF
		ENDIF
	ENDIF
	RETURN SELF

//-------------------------------------------------------------------------------------------
METHOD NodeToObj(oXML, pNode, hNS) CLASS oXML	// XML element => oXML

	LOCAL i, j, a, pNext
	LOCAL cName, cValue, cNs, cNsNew, hAttr, hNSNew, aNs

	IF IsNil(hNS);	hNS := { => };	ENDIF

	cName	:= mxmlGetElement(pNode)		// Element name
	cValue	:= mxmlGetOpaque(pNode)			// Element value
	hNSNew	:= hb_HClone(hNS)
	hAttr	:= { => }

	IF hb_mxmlGetAttrsCount( pNode ) > 0
		aNs := {}
		hAttr := hb_mxmlGetAttrs( pNode )	// Attributes hash
		FOR i:=1 TO Len(hAttr)			// NameSpaces normalisation
			a := HB_HPairAt(hAttr, i)	// {Name, Value}
			IF !(a[1] = "xmlns" .OR. a[1] = "targetNamespace");	LOOP;	ENDIF
			cNs := ""			// Name of NameSpace
			IF (j := At(":", a[1])) > 0
				cNs := Substr(a[1], j+1)
			ENDIF
			AADD(aNs, {cNs, a[2], i})
		NEXT
		FOR i:=Len(aNs) TO 1 STEP -1		// To delete from hAttr without problems
			cNs := aNs[i,1]
			cNsNew := ::GetStdNs(cNs, aNs[i,2])
			IF !(cNsNew == cNs)			// NS has standard name?
				hNSNew[cNs] := cNsNew
			ENDIF
			HB_HDelAt(hAttr, aNs[i,3])		// Don't need this attribute
		NEXT
	ENDIF
	
	cNs := ""					// Name of element's NameSpace: Let's replace
	IF (j := At(":", cName)) > 0			// it with normalized form
		cNs := Left(cName, j-1)
	ENDIF
	IF !IsNil(cNsNew := HB_HGetDef(hNSNew, cNs))
		cNsNew += ":"
		IF !Empty(cNs);	cNs += ":";	ENDIF
		cName := cNsNew + Substr(cName, Len(cNs) + 1)
	ENDIF

	oXML:cXmlName		:= cName
	oXML:aXmlElements	:= {}						// Node elements ( {oXML, ...} )
	IF !Empty(hAttr);	oXML:hXmlAttributes	:= hAttr;	ENDIF	// Node attributes ( { name => value, ... } )
	IF !Empty(cValue);	oXML:cXmlValue		:= cValue;	ENDIF	// node "simple" value
	IF !Empty( pNext := mxmlWalkNext( pNode, pNode, MXML_DESCEND ) )
//		ShowNode(pNext, "WalkNext2:")
		DO WHILE !Empty(pNext)
			IF mxmlGetType(pNext) == MXML_ELEMENT
				::NodeToObj(oXml{oXml}, pNext, hNSNew)
			ENDIF
			pNext := mxmlGetNextSibling(pNext)
//			ShowNode(pNext, "NextSibling2:")
		ENDDO
	ENDIF
	RETURN .T.

/*---------- oXML:noIVarGet ------------------------------------------------15/04/18--------*/
METHOD noIVarGet( cName ) CLASS oXML			// oDS:xxx VAR Get
	LOCAL aFound, r:=""
	IF !Empty(aFound := ::GetNameInfo(cName))
		IF ValType(r := aFound[4]) = "A";	r := r[1];	ENDIF	// ALWAYS first object/Value
	ENDIF					
	RETURN r

/*---------- oXML:noIVarPut ------------------------------------------------15/04/18--------*/
METHOD noIVarPut(cName, uValue) CLASS oXML		// oDS:xxx VAR Assign
	LOCAL r:=""				// No matter what we return, we must assign here
	LOCAL aFound := ::GetNameInfo(cName)	// {Area, originalName, fullName, value}
	LOCAL a:=::ParseName(cName)
	LOCAL cArea	  			// Area to search name
//	LOCAL cMods				// Modifiers

	cArea := Left(a[1],1)  			// Area to search name
//	cMods := Substr(a[1],2)			// Modifiers
	cName := a[2]				// Normalized name

	IF !Empty(aFound)
		cArea := aFound[1]
		DO CASE
			CASE cArea = "E".AND.IsNil(uValue);	AEval(aFound[4], {|x| x := NIL})// := NIL: Delete All !!!
			CASE cArea = "E".AND.Len(aFound[4])>1;	ErrGen("Ambiguous object " + cName)
			CASE cArea = "E";			aFound[4,1]:Assign(uValue)	// Sic!!! To simplify assignments
//			CASE cArea = "E";			aFound[4,1] := uValue		// This doesn't work, Harbour doesn't care what is really in array.
//												// Just assigns uValue to aFound[4,1]
// !!! Maybe this is a bad idea - to assign directly without :Value

			CASE cArea = "A" 
				IF IsNil(uValue)
					hb_HDel(::hXmlAttributes, aFound[3])
				ELSE
					::hXmlAttributes[aFound[3]] := uValue
				ENDIF
			CASE cArea = "U" 
				IF IsNil(uValue)
					hb_HDel(::hXmlUserVars, aFound[3])
				ELSE
					::hXmlUserVars[aFound[3]] := uValue
				ENDIF
		ENDCASE
	ELSE
		DO CASE
			CASE cArea = "E" .AND. IsNil(uValue)			// Delete
			CASE cArea = "E";				ErrGen("No object to assign")
			CASE cArea = "A";				::hXmlAttributes[cName] := uValue
			CASE cArea = "U";				::hXmlUserVars[cName] := uValue
		ENDCASE

	ENDIF
	RETURN r

/*---------- oXML:onError --------------------------------------------------15/04/18--------*/
METHOD onError( ... ) CLASS oXML

	LOCAL cMsg := __GetMessage(), r := Nil
	DO CASE
		CASE pCount() == 0					// VAR Get or Method without parameters
			r := ::noIVarGet(cMsg)

		CASE Left(cMsg, 1) == "_" .AND. PCount() == 1		// Unknown VAR Assign
			r := ::noIVarPut( Substr(cMsg, 2), hb_pValue(1) )
//		CASE hb_IsFunction( cMsg )				// Unknown ::Method(...) => dbf Function(...)
//		CASE TYPE(cMsg + "()") = "UI"				// Harbour 3.0 - no hb_IsFunction yet
//			c := "{|...| " + wAlias + "->(" + cMsg + "( ... ))}"
//			cb := &(c)
 //    			r := Eval( cb, ... )
		OTHERWISE
			ErrGen("Invalid class member " + ::className() + ":" + cMsg)
			r := Nil
	ENDCASE
	RETURN r

//-------------------------------------------------------------------------------------------
METHOD OperatorIndex(index) CLASS oXML	// Get i-th sibling of SELF, starting from 1
	LOCAL j, oXml, a:={}, oParent := ::oXmlParent
	IF IsNil(index);	index := 1;	ENDIF
	DO CASE
		CASE ValType(index) = "C";	oXml := ::FindElement(index)	// This is path
		CASE index = 1;			oXml := SELF
		CASE IsNil(oParent);		ErrGen("No siblings for root")
		OTHERWISE
			FOR j:=1 TO Len(oParent:aXmlElements)
				IF oParent:aXmlElements[j]:cXmlName == ::cXmlName
					AAdd(a, oParent:aXmlElements[j])
				ENDIF
			NEXT
			IF index >= 1 .AND. index <= Len(a)
				oXml := a[index]
			ELSE
				ErrGen("No such element: " + Ntrim(index))
			ENDIF
			
	ENDCASE
	RETURN oXml

//-------------------------------------------------------------------------------------------
METHOD OperatorMinus(uValue) CLASS oXML	// Same as :=, but new oblect replaces existing and implies itself into oXML tree
	LOCAL r := SELF
	DO CASE
		CASE IsNil(uValue);					r := ::Delete()		// := NIL: Delete
		CASE ValType(uValue) = "O" .AND. SELF == uValue					// := SELF
		CASE ValType(uValue) = "O";				r := ::Replace(uValue)	// := oXml
		CASE Len(::aXmlElements) > 0;				ErrGen("Node has elements " + ::cXmlName)
		OTHERWISE;						::Value := uValue	// Sic!!! To simplify assignments
// !!! Maybe this is a bad idea - to assign directly without :Value
	ENDCASE
	RETURN r

//-------------------------------------------------------------------------------------------
METHOD OperatorPlus(uValue) CLASS oXML	// Add new son to SELF
	DO CASE
		CASE IsNil(uValue);			::DelSons()		// += NIL
		CASE ValType(uValue) = "O";		::AddSon(uValue)	// := oXml
		CASE !(ValType(uValue) = "C");		ErrGen("Bad + operand " + hb_ValToExp(uValue))
		CASE !(":" $ uValue);			ErrGen("No namespace in " + uValue)
		OTHERWISE;				oXml():New(SELF, uValue)
	ENDCASE
	RETURN SELF

//-------------------------------------------------------------------------------------------
METHOD Parent(oXml) CLASS oXML	// Access/Assign oXmlParent
//	LOCAL Top
	IF !IsNil(oXml)
		::DelParent()
		IF hb_AScan(oXml:aXmlElements, SELF,,, .T.) = 0
			AAdd(oXml:aXmlElements, SELF)
			::oXmlParent := oXml
		ENDIF
	ENDIF
	RETURN ::oXmlParent

//-------------------------------------------------------------------------------------------
METHOD ParseName( cName ) CLASS oXML	// Node name parsing, {Area [EAU], Mmodifiers [V], Normalized name}
					// Original name: [modifiers][areas_][nsName_]nodeName
					//	modifiers:	V Get node "value" rather than node object
					//	areas:		E	Search for nodename in Elements
					//			A	Search for nodename in Attributes
					//			U	Search for nodename in User vars
					//			no flags = All areas (EAU)
					//	nsName:		Standard namespace from ::aXmlNs
					//	nodeName:	To find/replace: Right unique part of node name
					//			To add new element: full node name (needs nsName also)
					//	result: {cModifiers+cAreas, nsName:nodeName}

	LOCAL j, c, cArea:="", cMods:="", cWhere:="EAU"

	DO WHILE cName = "_";					cName := Substr(cName,2);	ENDDO	// Just ignore first "_"
	FOR j:=1 TO Len(cName)
		c := Upper(Substr(cName, j, 1))
		DO CASE
//			CASE c == "V";		cMods += c	// Get oXml:Value, not oXML object
			CASE c $ cWhere;	cArea += c	// Area (EAU)
			CASE c = "_"				// End of cArea + cMode
				cName := Substr(cName, Len(cMods + cArea) + 2)
				EXIT
			OTHERWISE				// Wrong structure
				cMods := ""
				cArea := ""
				EXIT
		ENDCASE
	NEXT
	DO WHILE cName = "_";					cName := Substr(cName,2);	ENDDO	// Just ignore first "_"

	IF (j:=At("_", cName)) > 0				// Is there Namespace prefix (ns_name, instead of ns:name)
		IF hb_AScan(::aXMLns, {|x| Upper(x[1]) == Upper(Left(cName, j-1))}) > 0
			cName := Stuff(cName, j, 1, ":")	// ns_name => ns:name
			cArea := "E"				// nameSpace - for elements only
		ENDIF
	ENDIF

	IF Empty(cArea);	cArea := "*";	ENDIF		// Area = anywhere
	RETURN {cArea + cMods, cName}		

//-------------------------------------------------------------------------------------------
METHOD Replace(oXml, lSaveName) CLASS oXML	// Replaces SELF with oXml
	LOCAL oParent := ::Parent, j
	IF IsNil(lSaveName);	lSaveName := .T.;	ENDIF	// Replaced node preserves its name

	IF ValType(oXml) = "C";	oXml := oXml{, oXml};	ENDIF	// Non-root

	IF !(ValType(oXml) = "O" .AND. oXml == SELF)
		IF !Empty(oParent)
			j := hb_AScan(oParent:aXMLElements, SELF,,,.T.)
			::Delete()
			IF !IsNil(oXml)
				oXml := oParent:AddSon(oXml, j)		// oXml is cloned in AddSon
				IF lSaveName
					oXml:cXmlName := ::cXmlName	// Restore Original node name from replaced node
				ENDIF
			ENDIF
		ELSE
			ErrGen("Cannot replace root: " + ToString(oXml))
		ENDIF
	ENDIF
	RETURN oXml

//-------------------------------------------------------------------------------------------
METHOD Top() CLASS oXML		// Get root node
	LOCAL oTop := SELF, oNext
	DO WHILE !Empty(oNext := oTop:oXmlParent);	oTop := oNext;	ENDDO
	RETURN oTop

//-------------------------------------------------------------------------------------------
METHOD ToXML(aStdNs, sXml, iOff) CLASS oXML		// Put oXML tree to sXml
							// aStdNs: {}|.T. "root" form (+headers+namespaces) (.T.=::aStdNs)
	LOCAL j, y, cAttr, sAttr, kOff:=3, cName, cValue, lNeedsRoot:=.F.

	IF ValType(aStdNs) = "L";	aStdNs := IIF(aStdNs, ::aXmlNs, NIL);	ENDIF
	IF IsNil(sXml)					// Root node
		sXml := ""
		IF !IsNil(aStdNs)
			lNeedsRoot := .T.
			::AddNameSpaces(aStdNs)		// Temporarily add namespaces as root node attributes
			IF Empty(::cXmlHeader);	ErrGen("Header not defined");	ENDIF
			sXml += "<" + ::cXmlHeader + ">"
		ENDIF
	ENDIF
	IF IsNil(iOff);	iOff := 0;	ENDIF

	cName	:= ::cXmlName
	cValue	:= ::cXmlValue
	DO CASE
		CASE Empty(cValue)
		CASE !(ValType(cValue) = "C")
		CASE cValue = "?"
		CASE "UTF" $ Upper(::cXmlHeader)
			FOR j:=1 TO Len(::aTrans)
				cValue := hb_utf8StrTran(cValue, ::aTrans[j,1], ::aTrans[j,2])
			NEXT
		OTHERWISE
			FOR j:=1 TO Len(::aTrans)
				cValue := StrTran(cValue, ::aTrans[j,1], ::aTrans[j,2])
			NEXT
	ENDCASE

	cAttr	:= ""
	IF !(ValType(cValue) = "C" .AND. cValue == "?")	// Elements with Value = "?" are ignored
		IF Len(::hXmlAttributes) > 0
			FOR EACH y IN ::hXmlAttributes
				sAttr := y:__enumValue()
				IF !(sAttr = '"');	sAttr := '"' + sAttr + '"';	ENDIF
				cAttr += " " + y:__enumKey() + "=" + sAttr
			NEXT
		ENDIF
		IF !Empty(cName)
			sXml += IIF(Empty(sXml), "", CRLF) + Space(iOff * kOff) + "<" + cName + cAttr + ">"
			IF !Empty(cValue) .OR. Empty(::aXmlElements)
				IF IsNil(cValue);	cValue := "";	ENDIF
				sXml += cValue + "</" + cName + ">"
			ENDIF
		ENDIF
		IF !Empty(::aXmlElements)
			FOR j:=1 TO Len(::aXmlElements)
				sXml := ::aXmlElements[j]:ToXML(, sXml, iOff + 1)
			NEXT
			IF !Empty(cName)
				sXml += CRLF + Space(iOff * kOff) + "</" + cName + ">"
			ENDIF
		ENDIF
		IF lNeedsRoot	
			::DelNameSpaces()	// NS were added to root node temporarily, just to create XML string
		ENDIF
	ENDIF
	RETURN sXml

//-------------------------------------------------------------------------------------------
METHOD Value(uValue) CLASS oXML	// Access/Assign cXmlValue
//	LOCAL Top
	IF !IsNil(uValue)
		::cXmlValue := ToString(uValue)
	ENDIF
	RETURN ::cXmlValue

//-------------------------------------------------------------------------------------------
STATIC FUNCTION NTRIM(n)			// Выдача числа/числовой строки без ведущих пробелов
	LOCAL c
	IF ValType(n) $ "CM";	n := Val(AllTrim(n));	ENDIF
	c := AllTrim(STR(n,20,4))
	DO WHILE Right(c,1) = "0";	c := Left(c,Len(c)-1);	ENDDO
	IF Right(c,1) = ".";		c := Left(c,Len(c)-1);	ENDIF
	IF Empty(c);			c := "0";		ENDIF
	RETURN c

//-------------------------------------------------------------------------------------------
STATIC FUNCTION s_type_cb()
	RETURN MXML_OPAQUE

//-------------------------------------------------------------------------------------------
STATIC PROCEDURE my_mxmlError( cErrorMsg )
	? s_mxml_error_msg := cErrorMsg
	RETURN

//-------------------------------------------------------------------------------------------
METHOD ShowNode(pNode, cText) CLASS oXml	// Show Node info
	LOCAL c := cText
	IF !Empty(pNode)
		c += hb_ValToExp({cText, " type=", mxmlGetType(pNode), " name=", mxmlGetElement(pNode), " value=", mxmlGetOpaque(pNode)})
	ELSE
		c += " is empty"
	ENDIF
	outLog(7, c)
	RETURN .T.	

// ========== Log ======================================
STATIC FUNCTION outLog(iFlag, cMsg)		// Вывод сообщений
	LOCAL h, cPath
	IF IsSet(iFlag, iLog)
//		? cMsg				// Пока так
		IF Empty(cLogFile)
			cPath := hb_dirBase()
			cLogFile := cPath + "\errlog\errLog.txt"
			hb_dirBuild(cPath + "errlog")
			hb_memowrit(cLogFile, "")
		ENDIF

		IF File(cLogFile)
			h := FOPEN(cLogFile, FO_READWRITE + FO_SHARED)
			FSEEK(h, 0, FS_END)
			cMsg := DTOC(DATE()) +" " + TIME() + " oXml " + cMsg + CRLF
			FWRITE(h, cMsg)
			FCLOSE(h)
		ENDIF
	ENDIF
	RETURN .T.

STATIC FUNCTION mxmlFindFirst(pRoot, cMsg)		// Вывод дерева
	LOCAL pFound
	IF IsNil(cMsg); cMsg := "";	ENDIF
	IF !Empty(pRoot)
//		ShowNode(pRoot, cMsg)
		IF Empty(pFound := mxmlCheckNode(pRoot))
			pFound := mxmlFindFirst(mxmlWalkNext(pRoot, pRoot, MXML_DESCEND), cMsg)
			DO WHILE Empty(pFound) .AND. !Empty(pRoot := mxmlGetNextSibling(pRoot))
//				ShowNode(pRoot, cMsg + "   ")
				IF Empty(pFound := mxmlCheckNode(pRoot))
					pFound := mxmlFindFirst(mxmlWalkNext(pRoot, pRoot, MXML_DESCEND), cMsg + "   ")
				ENDIF
			ENDDO
		ENDIF
	ENDIF
//	IF !Empty(pFound);	ShowNode(pFound, "pFound = ");	ENDIF
	RETURN pFound

STATIC FUNCTION mxmlCheckNode(pRoot)		// Вывод дерева
	RETURN IIF(mxmlGetType( pRoot ) = MXML_ELEMENT .AND. !(mxmlGetElement(pRoot) = "?"), pRoot, NIL)
	
STATIC FUNCTION ErrGen(cMsg)			// Генерация Ошибки
	LOCAL oError := ErrorNew()
	oError:severity    := ES_ERROR
	oError:genCode     := EG_LIMIT
	oError:subSystem   := "oXML"
	oError:subCode     := 0
	oError:description := cMsg
	oError:canRetry    := .F.
	oError:canDefault  := .F.
	oError:fileName    := ""
	oError:osCode      := 0
	Eval( ErrorBlock(), oError )
//	return __errRT_SBASE( nSubCode, nCode, NIL, ::className + ":" + cMsg, 1, self )
	RETURN .T.

/*---------- ASSTRING ----------------------------------------------------------16.09.15--------*/
STATIC FUNCTION ToString(u)		// Any value => String
	LOCAL c:="???", t:=VALTYPE(u)
	DO CASE
		CASE u = NIL;	c := ""			// Sic!!!
		CASE t $ "CM";	c := ALLTRIM(u)
		CASE t = "D";	c := DTOC(u)
		CASE t = "N";	c := NTrim(u)
//		CASE t = "A";	c := ASTR(u)
		CASE t = "L";	c := IIF(u, "true", "false")
		CASE t = "O";	c := u:Value
	ENDCASE

 RETURN c

/*---------- ASSTRING ----------------------------------------------------------16.09.15--------*/
METHOD ToString(iType) CLASS oXml	// Presentation of oXml
	LOCAL i, c:="", a
	IF IsNil(iType);	iType := 0;	ENDIF
	DO CASE
		CASE iType = 0;	c := ::Value
		CASE iType = 1;	a := __objGetValueList(SELF)
				FOR i:=1 TO Len(a)
					c += Padr(a[i,1],15) + " = (" + ValType(a[i,2]) + ") " + ToString(a[i,2]) + CRLF
				NEXT
	ENDCASE
	RETURN c

/*---------- FindNameSpaces -------------------------------------------------------30.07.19--------*/
FUNCTION FindNameSpaces(cPath)	// Collect all existing namespaces from all files in (wsdl) folder cPath
// Collect all existing NAMED namespaces with their names and paths from all files in (wsdl) 
// folder cPath and put them to AllNameSpaces.txt, where each line contains NS path and 
// all its names found in cPath. This file cannot be used as NS list, but you can choose 
// unique NS name you like for every NS path (you can also create your own names), and 
// create Std NS file manually.
// "Real" Std NS text file contains lines, each line is like:
//	'xmlns:' + NSName + '="' + NSPath + '"' + CRLF, e.g.
//	xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"

	LOCAL i, j, iOff, c, s, cNs, cNsPath, a:={}, aDir := hb_DirScan(cPath, "*.*"), wMax:=0
	FOR i:=1 TO Len(aDir)
		c := hb_memoread(cPath + "\" + aDir[i,1])
		iOff := 1
		DO WHILE (iOff := hb_At("xmlns", c, iOff)) > 0
			iOff += 5
			IF Substr(c, iOff, 1) = ":";	iOff ++;	ENDIF
			IF (j := hb_At("=", c, iOff)) > 0
				IF Empty(cNs := Substr(c, iOff, j-iOff));	cNs := "??";	ENDIF
				iOff := j + 2
				IF (j := hb_At('"', c, iOff)) > 0
					cNsPath := Substr(c, iOff, j-iOff)
					iOff := j + 1
					IF (j := AScan(a, {|x| Upper(x[1]) == Upper(cNsPath)})) = 0
						AAdd(a, {cNsPath, {}})
						j := Len(a)
						wMax := Max(wMax, Len(cNsPath))
					ENDIF
					IF hb_AScan(a[j,2], cNs,,,.T.) = 0
						AAdd(a[j,2], cNs)
					ENDIF
				ENDIF
			ENDIF
		ENDDO
	NEXT
	FOR i:=1 TO Len(a)
		a[i,1] := Padr(a[i,1], wMax)
		a[i,2] := ASort(a[i,2])
	NEXT
	ASort(a,,,{|x,y| x[1] < y[1]})
	c := ""
	FOR i:=1 TO Len(a)
		s := hb_ValToExp(a[i,2])
		c += a[i,1] + Space(10) + Substr(Left(s, Len(s)-1),2) + CRLF
	NEXT
	hb_memowrit("AllNameSpaces.txt", c)
	RETURN ""

//-------------------------------------------------------------------------------------------
FUNCTION ANameSpaces(sXmlNs)	// Get array of NameSpaces {{nsName without ":", Path}} from file/string/path
	LOCAL i, j, c, s, aLines, aNs:={}
	
	IF ValType(sXmlNs) = "C"
		sXmlNs := AllTrim(sXmlNs)
		IF !(sXmlNs = "xmlns:")			// This is fileName, or path to wsdl
			IF File(sXmlNs)			// This is filename
				sXmlNs := AllTrim(hb_Memoread(sXmlNs))
			ELSE				// This is wsdl path, just to collect NSs to text file
				sXmlNs := FindNameSpaces(sXmlNs)	// to prepare "real" Std NS file manually
			ENDIF
		ENDIF
		IF sXmlNs = "xmlns:"
			aLines := hb_ATokens(sXmlNs, .T.)
			FOR i:=1 TO Len(aLines)
				IF !Empty(c := aLines[i]) .AND. c = "xmlns:" .AND. (j:=At("=",c)) > 7	// xmlns:x=...
					IF (s := Substr(c, j+1)) = '"'
						s := Substr(Left(s, Len(s)-1),2)
					ENDIF
					AAdd(aNs, {Substr(c, 7, j-7), s})
				ENDIF
			NEXT
		ENDIF
	ENDIF
	RETURN aNs

