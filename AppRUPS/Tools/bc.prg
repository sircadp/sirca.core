*********************************************************************
*
*  Visual FoxPro Functions for Bar Code Fonts 2.11
*  Copyright, IDAutomation.com, Inc. 2000. All rights reserved.
*
*  Visit http://www.BizFonts.com/vba/ for more information.
*
*  You may incorporate our Source Code in your application
*  only if you own a valid Multi-user, Corporate or Distribution
*  license from IDAutomation.com, Inc. for the associated font and
*  the copyright notices are not removed from the source code.
*
*  Distributing our source code or fonts outside your
*  organization requires a distribution license.
*
*  To find a particular function, search the text for it.
*
*  HOW TO USE in Visual FoxPro:
*  For best results, just add this file as a program module
*  to your VFP project, set it as a procedure file using
*  SET PROCEDURE TO .. ADDITIVE command
*  somewhere in the main (startup) code of the project
*  and access the function in your application as necessary.
*
*  To do this, rename this file to IDAutomationVBA.PRG
*  Then, in your VFP project manager select "Code" pane,
*  press "Add..." button and select this file.
*  Then put following command in the startup code of your application:
*  SET PROCEDURE TO IDAutomationVBA.PRG ADDITIVE
*
*  Example (type in command window):
*     SET PROCEDURE TO IDAutomationVBA.PRG ADDITIVE
*     ? Code128b("123456789")
*
*********************************************************************


* constant definitions for functions
#DEFINE NUMCHARS "0123456789"
#DEFINE ALPHACHARS "ABCDEFGHIJKLMNOPQRSTUVWXWZ"
&& following is encoding table for UPC* function, A and B sections
#DEFINE UPCaNUMTOCODE_A Chr(34)+Chr(35)+Chr(36)+Chr(37)+Chr(38)+Chr(44)+Chr(46)+Chr(47)+Chr(58)+Chr(59)
#DEFINE UPCaNUMTOCODE_B Chr(122)+Chr(61)+Chr(63)+Chr(64)+Chr(91)+Chr(92)+Chr(93)+Chr(95)+Chr(123)+Chr(125)

&& following is an encoding table for Code39Mod43:
&& code = order number of character and vise versa
&& This is also used to verify correct content of the string and
&& remove all extra characters
#DEFINE CHARSCode39Mod43 "0123456789ABCDEFGHIJKLMNOPQRSTUVWXWZ-. $/+%"

&& Codabar characters
#DEFINE CHARSCodabar "0123456789$+-/.:"

&& followingare code converting arrays for RM4SCC function to make correlation
&& between a character in the input string and R+C code.
#DEFINE CHARS_RM4SCC "0123456789ABCDEFGHIJKLMNOPQRSTUVWXWZ"
#DEFINE CODES_RM4SCC_RplusC chr(11)+chr(12)+chr(13)+chr(14)+chr(15)+chr(10)+chr(21)+chr(22)+chr(23)+chr(24)+chr(25)+chr(20)+chr(31)+chr(32)+chr(33)+chr(34)+chr(35)+chr(30)+chr(41)+chr(42)+chr(43)+chr(44)+chr(45)+chr(40)+chr(51)+chr(52)+chr(53)+chr(54)+chr(55)+chr(50)+chr(01)+chr(02)+chr(03)+chr(04)+chr(05)+chr(00)




procedure Postnet
    lparameters pcDataToEncode, pnReturnType
    * Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
    * For more info visit http://www.BizFonts.com or http://www.IDautomation.com
    *
    * The purpose of this code is to calculate the POSTNET barcode
    * Enter all the numbers without dashes

    m.lcDataToPrint = ""
    * Check to make sure data is numeric and remove dashes, etc.
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")

&& <<<< Calculate Check Digit >>>>
    m.lnweightedTotal = 0
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength
&& Get the value of each number
        m.lnCurrentCharNum = SubStr(m.pcDataToEncode, m.I, 1)
&& add the values together
        m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum)
    Next m.I
&& Find the m.lnCheckDigit by finding the number + m.lnweightedTotal that = a multiple of 10
&& divide by 10, get the remainder and subtract from 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) % 10
&& Get Printable String
    m.lcDataToPrint = m.pcDataToEncode
&& pnReturnType 0 returns data formatted to the barcode font
    If m.pnReturnType = 0 Then
        return "(" + m.lcDataToPrint + allt(str(m.lnCheckDigit)) + ")" + " "
    endif
&& pnReturnType 1 returns data formatted for human readable text
    If m.pnReturnType = 1
        return m.lcDataToPrint + allt(str(m.lnCheckDigit))
    endif
&& pnReturnType 2 returns the  check digit for the data supplied
    If m.pnReturnType = 2
        return Str(m.lnCheckDigit,2,0)
    endif
Endproc


procedure Code128
    lparameters lcDataToFormat, pnReturnType

&&  Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.IDAutomation.com
&&
&&  You may use our source code in your applications only if you are using barcode fonts created by IDautomation.com, Inc.
&&  and you do not remove the copyright notices in the source code.
&&
&&  The purpose of this code is to calculate the Code 128 barcode for ANY character set
&&
&&  Encode UCC/EAN 128 by inserting ASCII 202 into the string to encode
&&
&&  You MUST use the fully functional Code 128 (dated 12/2000 or later)
&&  font for this code to create and print a proper barcode
&&
    m.pcDataToEncode = ""
    m.lcDataToFormat = alltrim(m.lcDataToFormat)
    m.lcC128_StartA = Chr(203)
    m.lcC128_StartB = Chr(204)
    m.lcC128_StartC = Chr(205)
    m.lcC128_Stop = Chr(206)
&& Here we select character set A, B or C for the START character
    m.lnStringLength = Len(m.lcDataToFormat)
    m.lnCurrentCharNum = Asc(left(m.lcDataToFormat, 1))
    m.lcC128Start = iif(m.lnCurrentCharNum = 202, m.lcC128_StartC, ;
        iif((m.lnStringLength > 4) AND IsNumber(left(m.lcDataToFormat, 4)), m.lcC128_StartC, ;
        iif(between(m.lnCurrentCharNum, 32, 126), m.lcC128_StartB, ;
        iif(m.lnCurrentCharNum < 32, m.lcC128_StartA, "" )))) &&  !!! used "" by default !!!
&&  202 is the Fnc1, with this Start C is mandatory above

    m.lcCurrentEncoding = chrtran(m.lcC128Start, ;
        m.lcC128_StartA + m.lcC128_StartB + m.lcC128_StartC, ;
        "ABC")

    For m.I = 1 To m.lnStringLength
        m.lcCurChar = SubStr(m.lcDataToFormat, m.I, 1)
        do case
&& check for m.lcFnc1 in any set
            case m.lcCurChar = Chr(202)
                m.pcDataToEncode = m.pcDataToEncode + Chr(202)
&& check for switching to character set C
            case ((m.I < m.lnStringLength - 2) AND ;
                    IsNumber(SubStr(m.lcDataToFormat, m.I, 1)) ;
                    	AND IsNumber(SubStr(m.lcDataToFormat, m.I+1, 1)) ;
                    	AND IsNumber(SubStr(m.lcDataToFormat, m.I, 4))) ;
                    Or ((m.I < m.lnStringLength) And ;
                    	IsNumber(SubStr(m.lcDataToFormat, m.I, 1)) AND IsNumber(SubStr(m.lcDataToFormat, m.I+1, 1)) AND ;
                    (m.lcCurrentEncoding == "C"))
&& switch to set C if not already in it
                If NOT (m.lcCurrentEncoding == "C")
                    m.pcDataToEncode = m.pcDataToEncode + Chr(199)
                endif
                m.lcCurrentEncoding = "C"
                m.lcCurrentChar = SubStr(m.lcDataToFormat, m.I, 2)
                m.lnCurrentValue = val(m.lcCurrentChar)
&& set the m.lnCurrentValue to the number of String m.lcCurrentChar
                m.pcDataToEncode = m.pcDataToEncode + Chr( ;
                    iif(m.lnCurrentValue = 0, 194, ;
                    iif(m.lnCurrentValue > 94, m.lnCurrentValue + 100, ;
                    m.lnCurrentValue + 32) ))
                m.I = m.I + 1
&& check for switching to character set A
            case (m.I <= m.lnStringLength) And ;
                    ((Asc(m.lcCurChar) < 31) Or ;
                    ((m.lcCurrentEncoding = "A") And ;
                    (between(Asc(m.lcCurChar), 33, 95))))

&& switch to set A if not already in it
                If NOT (m.lcCurrentEncoding == "A")
                    m.pcDataToEncode = m.pcDataToEncode + Chr(201)
                endif
                m.lcCurrentEncoding = "A"
&& Get the ASCII value of the next character
                m.lnCurrentCharNum = Asc(m.lcCurChar)
                m.pcDataToEncode = m.pcDataToEncode + Chr( ;
                    iif(m.lnCurrentCharNum = 32, 194, ;
                    iif(m.lnCurrentCharNum < 32, m.lnCurrentCharNum + 96, ;
                    m.lnCurrentCharNum)) )
&& check for switching to character set B
            case (m.I <= m.lnStringLength) And ;
                    between(Asc(m.lcCurChar),32,126)

&& switch to set B if not already in it
                If NOT (m.lcCurrentEncoding == "B")
                    m.pcDataToEncode = m.pcDataToEncode + Chr(200)
                endif
                m.lcCurrentEncoding = "B"
&& Get the ASCII value of the next character
                m.lnCurrentCharNum = Asc(m.lcCurChar)
                m.pcDataToEncode = m.pcDataToEncode + Chr( ;
                    iif(m.lnCurrentCharNum = 32,194,m.lnCurrentCharNum))
        endcase
    Next m.I

    m.lcHumanReadableText = ""
&&  FORMAT TEXT FOR AIs
    For m.I = 1 To m.lnStringLength
&& Get ASCII value of each character
        m.lnCurrentCharNum = Asc(SubStr(m.lcDataToFormat, m.I, 1))
&& Check for m.lcFnc1
        do case
            case (m.I < m.lnStringLength - 2) And (m.lnCurrentCharNum = 202)
&& It appears that there is an AI
&& Get the value of each number pair (ex: 5 and 6 = 5*10+6 =56)
                m.lcCurrentChar = (SubStr(m.lcDataToFormat, m.I + 1, 2))
                m.lnCurrentCharNum = val(m.lcCurrentChar)
                do case
&& Is 4 digit AI?
                    case (m.I < m.lnStringLength - 4) And (between(m.lnCurrentCharNum,80,81) Or between(m.lnCurrentCharNum,31,34))
                        m.lcHumanReadableText = m.lcHumanReadableText + " (" + (SubStr(m.lcDataToFormat, m.I + 1, 4)) + ") "
                        m.I = m.I + 4
&& Is 3 digit AI?
                    case (m.I < m.lnStringLength - 3) And (between(m.lnCurrentCharNum,40,49) Or between(m.lnCurrentCharNum,23,25))
                        m.lcHumanReadableText = m.lcHumanReadableText + " (" + (SubStr(m.lcDataToFormat, m.I + 1, 3)) + ") "
                        m.I = m.I + 3
&& Is 2 digit AI?
                    case between(m.lnCurrentCharNum,0,30) Or between(m.lnCurrentCharNum,90,99)
                        m.lcHumanReadableText = m.lcHumanReadableText + " (" + (SubStr(m.lcDataToFormat, m.I + 1, 2)) + ") "
                        m.I = m.I + 2
                endcase
            case m.lnCurrentCharNum < 32
                m.lcHumanReadableText = m.lcHumanReadableText + " "
            case between(m.lnCurrentCharNum,32,127)
                m.lcHumanReadableText = m.lcHumanReadableText + chr(m.lnCurrentCharNum)
        endcase
    Next m.I
    m.lcDataToFormat = ""

&& <<<< Calculate Modulo 103 Check Digit >>>>
&& Set m.lnweightedTotal to the value of the start character
    m.lnweightedTotal = (Asc(m.lcC128Start) - 100)
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength
&& Get the ASCII value of each character
        m.lnCurrentCharNum = Asc(SubStr(m.pcDataToEncode, m.I, 1))
&& Get the Code 128 value of m.lcCurrentChar according to chart
        m.lnCurrentValue = iif(m.lnCurrentCharNum = 194, 0, ;
            iif(m.lnCurrentCharNum < 135, m.lnCurrentCharNum - 32, ;
            m.lnCurrentCharNum - 100) )
&& multiply by the weighting character
&& and add the values together
        m.lnweightedTotal = m.lnweightedTotal + m.lnCurrentValue * m.I
    Next m.I

&& divide the m.lnweightedTotal by 103 and get the remainder, this is the m.lnCheckDigitValue
    m.lnCheckDigitValue = m.lnweightedTotal % 103

&& Now that we have the m.lnCheckDigitValue, find the corresponding ASCII character from the table
    m.lcC128_CheckDigit = chr(iif(m.lnCheckDigitValue = 0, 194, ;
        iif(m.lnCheckDigitValue < 95, m.lnCheckDigitValue + 32, ;
        m.lnCheckDigitValue + 100)))
&& Check for spaces or "00" and print ASCII 194 instead
&& place changes in m.lcDataToPrint
    m.lcDataToPrint = chrtran(m.pcDataToEncode," ",Chr(194))
&& Get Printable String
    m.lcPrintable_string = m.lcC128Start + m.lcDataToPrint + m.lcC128_CheckDigit + m.lcC128_Stop + " "
    m.pcDataToEncode = ""
    m.lcDataToPrint = ""
&& m.pnReturnType 0 returns data formatted to the barcode font
    If m.pnReturnType = 0
        return m.lcPrintable_string
    endif
&& m.pnReturnType 1 returns data formatted for human readable text
    If m.pnReturnType = 1
        return m.lcHumanReadableText
    endif
&& m.pnReturnType 2 returns the check digit for the data supplied
    If m.pnReturnType = 2
        return m.lcC128_CheckDigit
    endif
endproc



procedure Code128a
    lparameters pcDataToEncode
&&
&&  This module is Copyright, IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com or http://www.IDautomation.com
&&
&&  The purpose of this code is to print the Code 128 barcode from character set A
&&  Use the characters from set B to print characters not on the keyboard
&&  The scanner will scan characters from set A
&&
&&  You MUST use the fully functional Code 128 (dated 12/2000 or later)
&&  font for this code to create and print a proper barcode
&&
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
    m.lcC128_StartA = Chr(203)
    m.lcC128_StartB = Chr(204)
    m.lcC128_StartC = Chr(205)
    m.lcC128_Stop = Chr(206)
&& Here we select character set A
    m.lcC128Start = m.lcC128_StartA
&& <<<< Calculate Modulo 103 Check Digit >>>>
&& Set m.lnweightedTotal to the value of the start character
    m.lnweightedTotal = Asc(m.lcC128Start) - 100
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength
&& Get the ASCII value of each character
        m.lnCurrentCharNum = (Asc(SubStr(m.pcDataToEncode, m.I, 1)))
&& Get the Code 128 value of m.lcCurrentChar according to chart
        m.lnCurrentValue = iif(m.lnCurrentCharNum < 135, m.lnCurrentCharNum - 32, m.lnCurrentCharNum - 100)
&& multiply by the weighting character
&& and add the values together
        m.lnweightedTotal = m.lnweightedTotal + m.lnCurrentValue * m.I
    Next m.I
&& divide the m.lnweightedTotal by 103 and get the remainder, this is the m.lnCheckDigitValue
    m.lnCheckDigitValue = m.lnweightedTotal % 103
&& Now that we have the m.lnCheckDigitValue, find the corresponding ASCII character from the table
    m.lcC128_CheckDigit = chr(iif(m.lnCheckDigitValue = 0, 194, ;
        iif(m.lnCheckDigitValue < 95, m.lnCheckDigitValue + 32, m.lnCheckDigitValue + 100)))
&& Check for spaces or "00" and print ASCII 194 instead
&& place changes in m.lcDataToPrint
    m.lcDataToPrint = chrtran(m.pcDataToEncode, " ", Chr(194))
&& Get PrintableString
    m.lcPrintable_string = m.lcC128Start + m.lcDataToPrint + m.lcC128_CheckDigit + m.lcC128_Stop + " "
&& Return the PrintableString
    return m.lcPrintable_string
endproc


procedure Code128b
    lparameters pcDataToEncode
&&
&&  Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com or http://www.IDautomation.com
&&
&&  You may use our source code in your applications only if you are using barcode fonts created by IDautomation.com, Inc.
&&  and you do not remove the copyright notices in the source code.
&&
&&  The purpose of this code is to calculate the Code 128 barcode from character set B
&&
&&  You MUST use the fully functional Code 128 (dated 12/2000 or later)
&&  font for this code to create and print a proper barcode
&&
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
    m.lcC128_StartA = Chr(203)
    m.lcC128_StartB = Chr(204)
    m.lcC128_StartC = Chr(205)
    m.lcC128_Stop = Chr(206)
&& Here we select character set A or B but not C
    m.lcC128Start = m.lcC128_StartB
&& <<<< Calculate Modulo 103 Check Digit >>>>
&& Set m.lnweightedTotal to the value of the start character
    m.lnweightedTotal = Asc(m.lcC128Start) - 100
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength
&& Get the ASCII value of each character
        m.lnCurrentCharNum = (Asc(SubStr(m.pcDataToEncode, m.I, 1)))
&& Get the Code 128 value of m.lcCurrentChar according to chart
        m.lnCurrentValue = iif(m.lnCurrentCharNum < 135, m.lnCurrentCharNum - 32, m.lnCurrentCharNum - 100)
&& multiply by the weighting character
&& and add the values together
        m.lnweightedTotal = m.lnweightedTotal + m.lnCurrentValue * m.I
    Next m.I
&& divide the m.lnweightedTotal by 103 and get the remainder, this is the m.lnCheckDigitValue
    m.lnCheckDigitValue = m.lnweightedTotal % 103
&& Now that we have the m.lnCheckDigitValue, find the corresponding ASCII character from the table
    m.lcC128_CheckDigit = chr(iif(m.lnCheckDigitValue = 0, 194, ;
        iif(m.lnCheckDigitValue < 95, m.lnCheckDigitValue + 32, m.lnCheckDigitValue + 100)))
&& Check for spaces or "00" and print ASCII 194 instead
&& place changes in m.lcDataToPrint
    m.lcDataToPrint = chrtran(m.pcDataToEncode, " ", Chr(194))
&& Get Printable String
    m.lcPrintable_string = m.lcC128Start + m.lcDataToPrint + m.lcC128_CheckDigit + m.lcC128_Stop + " "
&& Return the PrintableString
    return m.lcPrintable_string
endproc


procedure Code128c
    lparameters pcDataToEncode, pnReturnType
&&
&&  Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com
&&
&&  You may use our source code in your applications only if you are using barcode fonts created by IDautomation.com, Inc.
&&  and you do not remove the copyright notices in the source code.
&&
&&  The purpose of this code is to calculate the Code 128 barcode from character set C
&&
&&  You MUST use the fully functional Code 128 (dated 12/2000 or later)
&&  font for this code to create and print a proper barcode
&&
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
&&  Check to make sure data is numeric
&&  Check to make sure data is numeric and remove dashes, etc.
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& Check for an even number of digits, add 0 if not even
    If Len(m.pcDataToEncode) % 2 = 1
        m.pcDataToEncode = "0" + m.pcDataToEncode
    endif
&& Assign start + stop codes
    m.lcStartCode = Chr(205)
    m.lcStopCode = Chr(206)
&& <<<< Calculate Modulo 103 Check Digit and generate m.lcDataToPrint >>>>
&& Set m.lnweightedTotal to the Code 128 value of the start character
    m.lnweightedTotal = 105
    m.lnWeightValue = 1
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength Step 2
&& Get the value of each number pair
        m.lnCurrentValue = val(SubStr(m.pcDataToEncode, m.I, 2))
&& get the m.lcDataToPrint
        m.lcDataToPrint = m.lcDataToPrint + Chr(iif(m.lnCurrentValue = 0, 194, ;
            iif(m.lnCurrentValue < 95, m.lnCurrentValue + 32, ;
            m.lnCurrentValue + 100)))
&& multiply by the weighting character
        m.lnCurrentValue = m.lnCurrentValue * m.lnWeightValue
&& add the values together to get the weighted total
        m.lnweightedTotal = m.lnweightedTotal + m.lnCurrentValue
        m.lnWeightValue = m.lnWeightValue + 1
    Next m.I
&& divide the m.lnweightedTotal by 103 and get the remainder, this is the m.lnCheckDigitValue
    m.lnCheckDigitValue = m.lnweightedTotal % 103
&& Now that we have the m.lnCheckDigitValue, find the corresponding ASCII character from the table
    m.lcC128_CheckDigit = chr(iif(m.lnCheckDigitValue = 0, 194, ;
        iif(m.lnCheckDigitValue < 95, m.lnCheckDigitValue + 32, m.lnCheckDigitValue + 100)))
&& m.pnReturnType 0 returns data formatted to the barcode font
    If m.pnReturnType = 0
        return m.lcStartCode + m.lcDataToPrint + m.lcC128_CheckDigit + m.lcStopCode + " "
    endif
&& m.pnReturnType 1 returns data formatted for human readable text
    If m.pnReturnType = 1
        return m.pcDataToEncode + alltrim(str(m.lnCheckDigitValue))
    endif
&& m.pnReturnType 2 returns the check digit for the data supplied
    If m.pnReturnType = 2
        return " " + alltrim(Str(m.lnCheckDigitValue))
    endif
endproc


procedure I2of5
    lparameters pcDataToEncode
&&
&&  Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com
&&
&&  You may use our source code in your applications only if you are using barcode fonts created by IDautomation.com, Inc.
&&  and you do not remove the copyright notices in the source code.
&&
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
&&  Check to make sure data is numeric and remove dashes, etc.
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& Check for an even number of digits, add 0 if not even
    If (Len(m.pcDataToEncode) % 2) = 1
        m.pcDataToEncode = "0" + m.pcDataToEncode
    endif
&& Assign start and stop codes
    m.lcStartCode = Chr(203)
    m.lcStopCode = Chr(204)
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength Step 2
&& Get the value of each number pair
        m.lnCurrentCharNum = Val(SubStr(m.pcDataToEncode, m.I, 2))
&& Get the ASCII value of m.lcCurrentChar according to chart by to the value
        m.lcDataToPrint = m.lcDataToPrint + Chr(iif(m.lnCurrentCharNum < 94, ;
            m.lnCurrentCharNum + 33, m.lnCurrentCharNum + 103))
    Next m.I
&& Get Printable String
    m.lcPrintable_string = m.lcStartCode + m.lcDataToPrint + m.lcStopCode + " "
&& Return PrintableString
    return m.lcPrintable_string
endproc



procedure USPSss
    lparameters pcDataToEncode, pnReturnType
&&  This code is Copyright, IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com
&&  The purpose for this code is to print a Code 128 barcode
&&  according to the USPS standards.

&&  Check to make sure data is numeric and remove dashes, etc.
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& Remove check digits and (AI) if they were added to input
    If Len(m.pcDataToEncode) = 20
        m.pcDataToEncode = left(m.lcOnlyCorrectData, 19)
    endif
&& End sub if incorrect number
    *     If Len(m.lcOnlyCorrectData) <> 19
    *		return
    *     endif

&& <<<< Generate MOD 10 check digit >>>>
    m.lnFactor = 3
    m.lnweightedTotal = 0
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = m.lnStringLength To 1 Step -1
&& Get the value of each number starting at the end
        m.lnCurrentCharNum = SubStr(m.pcDataToEncode, m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,1,3,1...
&& and add the sum together
        m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
        m.lnFactor = 4 - m.lnFactor
    Next m.I
&& Find the m.lnCheckDigit by finding the smallest number that = a multiple of 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) % 10
&& Now that we have calculated the MOD 10 for the data, send the string
&& to the Code128c() funtion. This function will:
&&  - Add in the start and stop codes
&&  - Calculate the MOD 103 required by SS when using Code 128
&&  - Interleave the numbers into printable characters
&& m.pnReturnType 0 returns data formatted to the barcode font
    If m.pnReturnType = 0
        return Code128c(m.pcDataToEncode + allt(str(m.lnCheckDigit)), 0)
    endif
&& m.pnReturnType 1 returns data formatted for human readable text
    If m.pnReturnType = 1
        return SubStr(m.pcDataToEncode, 1, 4) + " " + ;
            SubStr(m.pcDataToEncode, 5, 4) + " " + ;
            SubStr(m.pcDataToEncode, 9, 4) + " " + ;
            SubStr(m.pcDataToEncode, 13, 4) + " " + ;
            SubStr(m.pcDataToEncode, 17, 3) + allt(str(m.lnCheckDigit))
    endif
&& m.pnReturnType 2 returns the MOD10 check digit for the data supplied
    If m.pnReturnType = 2
        return " " + allt(str(m.lnCheckDigit))
    endif
endproc


procedure Code39Mod43
    lparameters pcDataToEncode, pnReturnType
&&
&&  This module is Copyright, IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com
&&
&&  The purpose of this code is to print the MOD43 CODE 39 barcode
    m.pcDataToEncode = Upper(RTrim(m.pcDataToEncode))
    m.lcDataToPrint = ""
    m.lcOnlyCorrectData = ""
&& only pass correct data
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, CHARSCode39Mod43, ""), "")
    m.lnweightedTotal = 0
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength
&& Get each character one at a time
        m.lcCurrentChar = SubStr(m.pcDataToEncode, m.I, 1)
&& Get the value of m.lcCurrentChar according to MOD43
&& Note that CHARSCode39Mod43 string contains characters already in the correct order.
&& We just use that order number as a code.
        m.lnCurrentValue = AT(m.lcCurrentChar, CHARSCode39Mod43)-1
&& To print the barcode symbol representing a space you will
&& to type or print "=" (the equal character) instead of a space character.
        If m.lcCurrentChar == " "
            m.lcCurrentChar = "="
        endif
&& gather data to print
        m.lcDataToPrint = m.lcDataToPrint + m.lcCurrentChar
&& add the values together
        m.lnweightedTotal = m.lnweightedTotal + m.lnCurrentValue
    Next m.I

&& divide the m.lnweightedTotal by 43 and get the remainder, this is the m.lnCheckDigit
    m.lnCheckDigitValue = m.lnweightedTotal % 43
&& Assign values to characters
&& Note that CHARSCode39Mod43 string contains characters already in the correct order.
&& We just use character at that order number
    m.lcCheckDigit = substr(CHARSCode39Mod43, m.lnCheckDigitValue + 1, 1)
    If m.lcCheckDigit == " "
        m.lcCheckDigit = "="
    endif
&& m.pnReturnType 0 returns data formatted to the barcode font
    If m.pnReturnType = 0
        return "!" + m.lcDataToPrint + m.lcCheckDigit + "!" + " "
    endif
&& m.pnReturnType 1 returns data formatted for human readable text
    If m.pnReturnType = 1
        return m.lcDataToPrint + m.lcCheckDigit
    endif
&& m.pnReturnType 2 returns the  check digit for the data supplied
    If m.pnReturnType = 2
        return m.lcCheckDigit
    endif
endproc



procedure Code39
    lparameters pcDataToEncode
&&  Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com
&&
&&  You may use our source code in your applications only if you are using barcode fonts created by IDautomation.com, Inc.
&&  and you do not remove the copyright notices in the source code.
&&
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
&& Check for spaces in code
&& To print the barcode symbol representing a space you will
&& to type or print "=" (the equal character) instead of a space character.
    m.lcDataToPrint = chrtran(m.pcDataToEncode," ","=")
&& Get Printable String
    m.lcPrintable_string = "!" + m.lcDataToPrint + "!" + " "
&& Return PrintableString
    return m.lcPrintable_string
endproc



procedure I2of5Mod10
    lparameters pcDataToEncode, pnReturnType
&&
&&  This module is Copyright, IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com or http://www.IDautomation.com
&&
&&  The purpose of this code is to print the Interleaved 2 of 5 barcode
&&  With a MOD 10 check digit. This is now required by the US Post Office for
&&  printing barcodes on US MAIL for their "Special Services". Use the AdvI25p
&&  font when printing barcodes for US MAIL.
&&
&&  Get data from user, this is the m.pcDataToEncode
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
    m.lcDataToPrint = ""
&&  Check to make sure data is numeric and remove dashes, etc.
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& <<<< Calculate Check Digit >>>>
    m.lnFactor = 3
    m.lnweightedTotal = 0
    For m.I = Len(m.pcDataToEncode) To 1 Step -1
&& Get the value of each number starting at the end
        m.lnCurrentCharNum = SubStr(m.pcDataToEncode, m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,1,3,1...
&& and add the sum together
        m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
        m.lnFactor = 4 - m.lnFactor
    Next m.I
&& Find the m.lnCheckDigit by finding the smallest number that = a multiple of 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) %10
&& Add check digit to number to m.pcDataToEncode
    m.pcDataToEncode = m.pcDataToEncode + alltrim(str(m.lnCheckDigit))
&& Check for an even number of digits, add 0 if not even
    If (Len(m.pcDataToEncode) % 2) = 1
        m.pcDataToEncode = "0" + m.pcDataToEncode
    endif
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength Step 2
&& Get the value of each number pair
        m.lnCurrentCharNum = val(SubStr(m.pcDataToEncode, m.I, 2))
&& Get the ASCII value of m.lcCurrentChar according to chart by to the value
        m.lcDataToPrint = m.lcDataToPrint + Chr(iif(m.lnCurrentCharNum < 94, ;
            m.lnCurrentCharNum + 33, m.lnCurrentCharNum + 103))
    Next m.I
&& m.pnReturnType 0 returns data formatted to the barcode font
    If m.pnReturnType = 0
        return Chr(203) + m.lcDataToPrint + Chr(204) + " "
    endif
&& m.pnReturnType 1 returns data formatted for human readable text
    If m.pnReturnType = 1
        return m.pcDataToEncode
    endif
&& m.pnReturnType 2 returns the  check digit for the data supplied
    If m.pnReturnType = 2
        return Str(m.lnCheckDigit,2,0)
    endif
endproc




procedure MSI
    lparameters pcDataToEncode, pnReturnType
&&
&&  Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com or http://www.IDautomation.com
&&
&&  You may use our source code in your applications only if you are using barcode fonts created by IDautomation.com, Inc.
&&  and you do not remove the copyright notices in the source code.
&&
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
&&  Check to make sure data is numeric and remove dashes, etc.
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& <<<< Calculate Check Digit >>>>
    m.lnFactor = 3
    m.lnweightedTotal = 0
    m.lnStringLength = Len(m.pcDataToEncode)
    m.lcOddNumbers = ""
    m.lnEvenNumberSum = 0
    For m.I = 1 To m.lnStringLength
        If m.lnFactor = 1
&& Get the value of each EVEN number, 1st number is even + add them together
            m.lnEvenNumberSum = m.lnEvenNumberSum + Val(SubStr(m.pcDataToEncode, m.I, 1))
        else
&& Get the value of each ODD number, 2nd number is odd + gether them
            m.lcOddNumbers = m.lcOddNumbers + SubStr(m.pcDataToEncode, m.I, 1)
        endif
        m.lnFactor = 4 - m.lnFactor
    Next m.I
&& Multiply odd number gathered by 2
    m.lcOddNumbers = allt(str(val(m.lcOddNumbers) * 2))
&& Add the digits of the product together
    m.lnOddProductSum = 0
    For m.I = 1 To Len(m.lcOddNumbers)
        m.lnOddProductSum = m.lnOddProductSum + Val(SubStr(m.lcOddNumbers, m.I, 1))
    Next m.I
    m.lnweightedTotal = m.lnOddProductSum + m.lnEvenNumberSum
&& Find the m.lnCheckDigit by finding the number + m.lnweightedTotal that = a multiple of 10
&& divide by 10, get the remainder and subtract from 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) % 10
&& m.pnReturnType 0 returns data formatted to the barcode font
    If m.pnReturnType = 0
        return "(" + m.pcDataToEncode + allt(str(m.lnCheckDigit)) + ")" + " "
    endif
&& m.pnReturnType 1 returns data formatted for human readable text
    If m.pnReturnType = 1
        return m.pcDataToEncode
    endif
&& m.pnReturnType 2 returns the  check digit for the data supplied
    If m.pnReturnType = 2
        return Str(m.lnCheckDigit,2,0)
    endif
endproc

procedure UPCa
    lparameters pcDataToEncode
&&
&&  Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com or http://www.IDautomation.com
&&
&&  You may use our source code in your applications only if you are using barcode fonts created by IDautomation.com, Inc.
&&  and you do not remove the copyright notices in the source code.
&&
&&  The purpose of this code is to calculate the UPC-A barcode
&&  Enter all the numbers without dashes
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
&&  Check to make sure data is numeric and remove dashes, etc.
    m.lcOnlyCorrectData = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& Remove check digits if they added one
    do case
        case Len(m.lcOnlyCorrectData) = 12
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 11)
        case Len(m.lcOnlyCorrectData) = 14
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 11) + SubStr(m.lcOnlyCorrectData, 13, 2)
        case Len(m.lcOnlyCorrectData) = 17
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 11) + SubStr(m.lcOnlyCorrectData, 13, 5)
    endcase
    m.lcEAN2AddOn = ""
    m.lcEAN5AddOn  = ""
    m.lcEANAddOnToPrint = ""
    do case
        case Len(m.lcOnlyCorrectData) = 13
            m.lcEAN2AddOn = SubStr(m.lcOnlyCorrectData, 12, 2)
        case Len(m.lcOnlyCorrectData) = 16
            m.lcEAN5AddOn  = SubStr(m.lcOnlyCorrectData, 12, 5)
    endcase
&& split 12 digit number from add-on
    m.pcDataToEncode = left(m.lcOnlyCorrectData, 11)
&& <<<< Calculate Check Digit >>>>
    m.lnFactor = 3
    m.lnweightedTotal = 0
    For m.I = Len(m.pcDataToEncode) To 1 Step -1
&& Get the value of each number starting at the end
        m.lnCurrentCharNum = SubStr(m.pcDataToEncode, m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,1,3,1...
&& and add the sum together
        m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
        m.lnFactor = 4 - m.lnFactor
    Next m.I
&& Find the m.lnCheckDigit by finding the number + m.lnweightedTotal that = a multiple of 10
&& divide by 10, get the remainder and subtract from 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) % 10
    m.pcDataToEncode = m.pcDataToEncode + alltrim(str(m.lnCheckDigit))
&& Now that have the total number including the check digit, determine character to print
&& for proper barcoding
    m.lnStringLength = Len(m.pcDataToEncode)

&& For the first character print the human readable character, the normal
&& guard pattern and then the barcode without the human readable character
    m.lnCurrentCharNum = asc(left(m.pcDataToEncode, 1))
    m.lcDataToPrint = iif(Chr(m.lnCurrentCharNum) > "4", ;
        Chr(m.lnCurrentCharNum + 64), ;
        Chr(m.lnCurrentCharNum + 37) ) + "(" + Chr(m.lnCurrentCharNum + 49)

    For m.I = 2 To min(m.lnStringLength,11)
&& Get the ASCII value of each number
        m.lnCurrentCharNum = asc(SubStr(m.pcDataToEncode, m.I, 1))
&& Print different barcodes according to the location of the m.lcCurrentChar:
&& - Print the center guard pattern after the 6th character
&& - Add 27 to the ASII value of characters 6-12 to print from character set+ C
&& this is required when printing to the right of the center guard pattern
        m.lcDataToPrint = m.lcDataToPrint + ;
            Chr(m.lnCurrentCharNum + iif(m.I>6,27,0)) + ;
            iif(m.I=6, "*", "")
    Next m.I
    if m.lnStringLength=12
&& For the last character print the barcode without the human readable character,
&& the normal guard pattern and then the human readable character.
	    m.lnCurrentCharNum = asc(right(m.pcDataToEncode, 1))
        m.lcDataToPrint = m.lcDataToPrint + Chr(m.lnCurrentCharNum + 59) + "(" + ;
            Chr(m.lnCurrentCharNum + iif(Chr(m.lnCurrentCharNum) > "4",64,37) )
    endif

&& Process 5 digit add on if it exists
    If Len(m.lcEAN5AddOn ) = 5
        m.lcEANAddOnToPrint = ""
&& Get check digit for add on
        m.lnFactor = 3
        m.lnweightedTotal = 0
        For m.I = 5 To 1 Step -1
&& Get the value of each number starting at the end
            m.lnCurrentCharNum = SubStr(m.lcEAN5AddOn , m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,9,3,9...
&& and add the sum together
            m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
            m.lnFactor = 12 - m.lnFactor
        Next m.I
&& Find the m.lnCheckDigit by extracting the right-most number from m.lnweightedTotal
        m.lnCheckDigit = m.lnweightedTotal % 10
&& Now we must encode the add-on m.lnCheckDigit into the number sets
&& by using variable parity between character sets A and B
        do case
            Case m.lnCheckDigit = 0
                m.lcEncoding = "BBAAA"
            Case m.lnCheckDigit = 1
                m.lcEncoding = "BABAA"
            Case m.lnCheckDigit = 2
                m.lcEncoding = "BAABA"
            Case m.lnCheckDigit = 3
                m.lcEncoding = "BAAAB"
            Case m.lnCheckDigit = 4
                m.lcEncoding = "ABBAA"
            Case m.lnCheckDigit = 5
                m.lcEncoding = "AABBA"
            Case m.lnCheckDigit = 6
                m.lcEncoding = "AAABB"
            Case m.lnCheckDigit = 7
                m.lcEncoding = "ABABA"
            Case m.lnCheckDigit = 8
                m.lcEncoding = "ABAAB"
            Case m.lnCheckDigit = 9
                m.lcEncoding = "AABAB"
        endcase
&& Now that we have the total number including the check digit, determine character to print
&& for proper barcoding:
        For m.I = 1 To 5
&& Get the value of each number
&& it is encoded with variable parity
            m.lcCurrentChar = SubStr(m.lcEAN5AddOn , m.I, 1)
            m.lcCurrentEncoding = SubStr(m.lcEncoding, m.I, 1)
&& Print different barcodes according to the location of the m.lcCurrentChar and m.lcCurrentEncoding
&& Note that here we use a converting table from digit to appropriate code
            m.lcEANAddOnToPrint = m.lcEANAddOnToPrint + ;
                chrtran(m.lcCurrentChar, NUMCHARS, ;
                iif(m.lcCurrentEncoding == "A", UPCaNUMTOCODE_A, UPCaNUMTOCODE_B ) )

&& add in the space + add-on guard pattern
            if m.I<5
&& Now print add-on delineators between each add-on character
                m.lcEANAddOnToPrint = iif(m.I=1,Chr(43),"") + m.lcEANAddOnToPrint + Chr(33)
            endif
        Next m.I
    endif

&& Process 2 digit add on if it exists
    If Len(m.lcEAN2AddOn) = 2 Then
        m.lcEANAddOnToPrint = ""
&& Get m.lcEncoding for add on
        m.lnAddOnNum = Val(m.lcEAN2AddOn) % 4
        m.lcEncoding = iif(m.lnAddOnNum>2,"B","A") + ;
            iif(m.lnAddOnNum % 2 = 1,"B","A")
&& Now that we have the total number including the m.lcEncoding
&& determine what to print
&& Get the value of each number
&& it is encoded with variable parity -- there are only 2 characters here

&& Print different barcodes according to the location of the m.lcCurrentChar and m.lcCurrentEncoding
&& Note that here we use a converting table from digit to appropriate code
&& add in the space + add-on guard pattern
        m.lcEANAddOnToPrint = ;
            Chr(43) + ;
            chrtran(left(m.lcEAN2AddOn, 1), NUMCHARS, ;
            iif(left(m.lcEncoding, 1) == "A", UPCaNUMTOCODE_A, UPCaNUMTOCODE_B ) ) + ;
            Chr(33) + ;
            chrtran(right(m.lcEAN2AddOn, 1), NUMCHARS, ;
            iif(right(m.lcEncoding, 1) == "A", UPCaNUMTOCODE_A, UPCaNUMTOCODE_B ) )
    endif

&& Get Printable String
    m.lcPrintable_string = m.lcDataToPrint + m.lcEANAddOnToPrint + " "
&& Return PrintableString
    return m.lcPrintable_string
endproc


procedure UPCe
    lparameters pcDataToEncode
&&
&&  This module is Copyright, IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com or http://www.IDautomation.com
&&
&&  The purpose of this code is to print the UPC-E barcode
&&  from a UPC-A barcode that can be compressed.
&&
&&  Get data from user, this is the m.pcDataToEncode
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
    m.lcDataToPrint = ""
&&  Check to make sure data is numeric and remove dashes, etc.
    m.lcOnlyCorrectData = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& Remove check digits if they added one
    do case
        case Len(m.lcOnlyCorrectData) = 12
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 11)
        case Len(m.lcOnlyCorrectData) = 14
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 11) + SubStr(m.lcOnlyCorrectData, 13, 2)
        case Len(m.lcOnlyCorrectData) = 17
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 11) + SubStr(m.lcOnlyCorrectData, 13, 5)
    endcase
    m.lcEAN2AddOn = ""
    m.lcEAN5AddOn  = ""
    m.lcEANAddOnToPrint = ""
    do case
        case Len(m.lcOnlyCorrectData) = 13
            m.lcEAN2AddOn = SubStr(m.lcOnlyCorrectData, 12, 2)
        case Len(m.lcOnlyCorrectData) = 16
            m.lcEAN5AddOn  = SubStr(m.lcOnlyCorrectData, 12, 5)
    endcase
    m.pcDataToEncode = left(m.lcOnlyCorrectData, 11)

&& <<<< Calculate Check Digit >>>>
    m.lnFactor = 3
    m.lnweightedTotal = 0
    For m.I = Len(m.pcDataToEncode) To 1 Step -1
&& Get the value of each number starting at the end
        m.lnCurrentCharNum = SubStr(m.pcDataToEncode, m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,1,3,1...
&& and add the sum together
        m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
        m.lnFactor = 4 - m.lnFactor
    Next m.I
&& Find the m.lnCheckDigit by finding the number + m.lnweightedTotal that = a multiple of 10
&& divide by 10, get the remainder and subtract from 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) % 10

    m.pcDataToEncode = m.pcDataToEncode + allt(str(m.lnCheckDigit))

&& Compress UPC-A to UPC-E if possible
    m.D1 = SubStr(m.pcDataToEncode, 1, 1)
    m.D2 = SubStr(m.pcDataToEncode, 2, 1)
    m.D3 = SubStr(m.pcDataToEncode, 3, 1)
    m.D4 = SubStr(m.pcDataToEncode, 4, 1)
    m.D5 = SubStr(m.pcDataToEncode, 5, 1)
    m.D6 = SubStr(m.pcDataToEncode, 6, 1)
    m.D7 = SubStr(m.pcDataToEncode, 7, 1)
    m.D8 = SubStr(m.pcDataToEncode, 8, 1)
    m.D9 = SubStr(m.pcDataToEncode, 9, 1)
    m.D10 = SubStr(m.pcDataToEncode, 10, 1)
    m.D11 = SubStr(m.pcDataToEncode, 11, 1)
    m.D12 = SubStr(m.pcDataToEncode, 12, 1)
&& Condition A
    If (m.D11 $ "56789") And m.D6 <> "0" And (m.D7 = "0" And m.D8 = "0" And m.D9 = "0" And m.D10 = "0")
        m.pcDataToEncode = m.D2 + m.D3 + m.D4 + m.D5 + m.D6 + m.D11
    endif
&& Condition B
    If (m.D6 = "0" And m.D7 = "0" And m.D8 = "0" And m.D9 = "0" And m.D10 = "0") And m.D5 <> "0"
        m.pcDataToEncode = m.D2 + m.D3 + m.D4 + m.D5 + m.D11 + "4"
    endif
&& Condition C
    If (m.D5 = "0" And m.D6 = "0" And m.D7 = "0" And m.D8 = "0") And (m.D4 $ "012")
        m.pcDataToEncode = m.D2 + m.D3 + m.D9 + m.D10 + m.D11 + m.D4
    endif
&& Condition D
    If (m.D5 = "0" And m.D6 = "0" And m.D7 = "0" And m.D8 = "0" And m.D9 = "0") And (m.D4 $ "3456789")
        m.pcDataToEncode = m.D2 + m.D3 + m.D4 + m.D10 + m.D11 + "3"
    endif
&&
&& Run UPC-E compression only if m.pcDataToEncode = 6
    If Len(m.pcDataToEncode) = 6 Then
&& Now we must encode the check character into the symbol
&& by using variable parity between character sets A and B
        do case
            Case m.D12 = "0"
                m.lcEncoding = "BBBAAA"
            Case m.D12 = "1"
                m.lcEncoding = "BBABAA"
            Case m.D12 = "2"
                m.lcEncoding = "BBAABA"
            Case m.D12 = "3"
                m.lcEncoding = "BBAAAB"
            Case m.D12 = "4"
                m.lcEncoding = "BABBAA"
            Case m.D12 = "5"
                m.lcEncoding = "BAABBA"
            Case m.D12 = "6"
                m.lcEncoding = "BAAABB"
            Case m.D12 = "7"
                m.lcEncoding = "BABABA"
            Case m.D12 = "8"
                m.lcEncoding = "BABAAB"
            Case m.D12 = "9"
                m.lcEncoding = "BAABAB"
        endcase
        m.lnStringLength = Len(m.pcDataToEncode)
        For m.I = 1 To m.lnStringLength
&& Get the ASCII value of each number
            m.lnCurrentCharNum = Asc(SubStr(m.pcDataToEncode, m.I, 1))
            m.lcCurrentEncoding = SubStr(m.lcEncoding, m.I, 1)
&& Print different barcodes according to the location of the m.lcCurrentChar and m.lcCurrentEncoding
            m.lcDataToPrint = m.lcDataToPrint + ;
                chr(m.lnCurrentCharNum + iif(m.lcCurrentEncoding=="A",0,17) )
&& add in the 1st character along with guard patterns
&& For the m.lnLeadingDigit print the human readable character,
&& the normal guard pattern and then the rest of the barcode
            if m.I=1
                m.lcDataToPrint = Chr(85) + "(" + m.lcDataToPrint
            endif
            if m.I=6
&& Print the SPECIAL guard pattern and check character for 6-th character
                m.lcDataToPrint = m.lcDataToPrint + ;
                    ")" + Chr(Asc(m.D12) + iif(m.D12>"4",64,37) )
            endif
        Next m.I
    endif

&& determine character to print
&& for proper upc-a barcoding
    If Len(m.pcDataToEncode) <> 6 Then
        m.lnStringLength = Len(m.pcDataToEncode)
        m.lnCurrentCharNum = Asc(left(m.pcDataToEncode, 1))
&& For the first character print the human readable character, the normal
&& guard pattern and then the barcode without the human readable character
        m.lcDataToPrint = Chr(m.lnCurrentCharNum + iif(Chr(m.lnCurrentCharNum) > "4",64,37) ) + ;
            "(" + Chr(m.lnCurrentCharNum + 49)
        For m.I = 2 To m.lnStringLength
&& Get the ASCII value of each number
            m.lnCurrentCharNum = Asc(SubStr(m.pcDataToEncode, m.I, 1))
&& Print different barcodes according to the location of the m.lcCurrentChar
&& Print the center guard pattern after the 6th character
&& Add 27 to the ASII value of characters 6-12 to print from character set+ C
&& this is required when printing to the right of the center guard pattern
            m.lcDataToPrint = m.lcDataToPrint + ;
                Chr(m.lnCurrentCharNum + iif(m.I>6,27,0)) + ;
                iif(m.I=6, "*", "")
        Next m.I
        if m.lnStringLength=12
&& For the last character print the barcode without the human readable character,
&& the normal guard pattern and then the human readable character.
		    m.lnCurrentCharNum = asc(right(m.pcDataToEncode, 1))
            m.lcDataToPrint = m.lcDataToPrint + Chr(m.lnCurrentCharNum + 59) + "(" + ;
                Chr(m.lnCurrentCharNum + iif(Chr(m.lnCurrentCharNum) > "4", 64, 37) )
        endif
    endif

&& Process 5 digit add on if it exists
    If Len(m.lcEAN5AddOn ) = 5
        m.lcEANAddOnToPrint = ""
&& Get check digit for add on
        m.lnFactor = 3
        m.lnweightedTotal = 0
        For m.I = 5 To 1 Step -1
&& Get the value of each number starting at the end
            m.lnCurrentCharNum = SubStr(m.lcEAN5AddOn , m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,9,3,9...
&& and add the sum together
            m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
            m.lnFactor = 12 - m.lnFactor
        Next m.I
&& Find the m.lnCheckDigit by extracting the right-most number from m.lnweightedTotal
        m.lnCheckDigit = m.lnweightedTotal % 10
&& Now we must encode the add-on m.lnCheckDigit into the number sets
&& by using variable parity between character sets A and B
        do case
            Case m.lnCheckDigit = 0
                m.lcEncoding = "BBAAA"
            Case m.lnCheckDigit = 1
                m.lcEncoding = "BABAA"
            Case m.lnCheckDigit = 2
                m.lcEncoding = "BAABA"
            Case m.lnCheckDigit = 3
                m.lcEncoding = "BAAAB"
            Case m.lnCheckDigit = 4
                m.lcEncoding = "ABBAA"
            Case m.lnCheckDigit = 5
                m.lcEncoding = "AABBA"
            Case m.lnCheckDigit = 6
                m.lcEncoding = "AAABB"
            Case m.lnCheckDigit = 7
                m.lcEncoding = "ABABA"
            Case m.lnCheckDigit = 8
                m.lcEncoding = "ABAAB"
            Case m.lnCheckDigit = 9
                m.lcEncoding = "AABAB"
        endcase
&& Now that we have the total number including the check digit, determine character to print
&& for proper barcoding:
        For m.I = 1 To 5
&& Get the value of each number
&& it is encoded with variable parity
            m.lcCurrentChar = SubStr(m.lcEAN5AddOn , m.I, 1)
            m.lcCurrentEncoding = SubStr(m.lcEncoding, m.I, 1)
&& Print different barcodes according to the location of the m.lcCurrentChar and m.lcCurrentEncoding
&& Note that here we use a converting table from digit to appropriate code
            m.lcEANAddOnToPrint = m.lcEANAddOnToPrint + ;
                chrtran(m.lcCurrentChar, NUMCHARS, ;
                iif(m.lcCurrentEncoding == "A", UPCaNUMTOCODE_A, UPCaNUMTOCODE_B ) )

&& add in the space + add-on guard pattern
            if m.I<5
&& Now print add-on delineators between each add-on character
                m.lcEANAddOnToPrint = iif(m.I=1,Chr(43),"") + m.lcEANAddOnToPrint + Chr(33)
            endif
        Next m.I
    endif

&& Process 2 digit add on if it exists
    If Len(m.lcEAN2AddOn) = 2 Then
        m.lcEANAddOnToPrint = ""
&& Get m.lcEncoding for add on
        m.lnAddOnNum = Val(m.lcEAN2AddOn) % 4
        m.lcEncoding = iif(m.lnAddOnNum>2,"B","A") + ;
            iif(m.lnAddOnNum % 2 = 1,"B","A")
&& Now that we have the total number including the m.lcEncoding
&& determine what to print
&& Get the value of each number
&& it is encoded with variable parity -- there are only 2 characters here

&& Print different barcodes according to the location of the m.lcCurrentChar and m.lcCurrentEncoding
&& Note that here we use a converting table from digit to appropriate code
&& add in the space + add-on guard pattern
        m.lcEANAddOnToPrint = ;
            Chr(43) + ;
            chrtran(left(m.lcEAN2AddOn, 1), NUMCHARS, ;
            iif(left(m.lcEncoding, 1) == "A", UPCaNUMTOCODE_A, UPCaNUMTOCODE_B ) ) + ;
            Chr(33) + ;
            chrtran(right(m.lcEAN2AddOn, 1), NUMCHARS, ;
            iif(right(m.lcEncoding, 1) == "A", UPCaNUMTOCODE_A, UPCaNUMTOCODE_B ) )
    endif

&& Get Printable String
    m.lcPrintable_string = m.lcDataToPrint + m.lcEANAddOnToPrint + " "

&& Return PrintableString
    return m.lcPrintable_string

endproc

procedure EAN13
    lparameters pcDataToEncode
&&
&&  Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com or http://www.IDautomation.com
&&
&&  You may use our source code in your applications only if you are using barcode fonts created by IDautomation.com, Inc.
&&  and you do not remove the copyright notices in the source code.
&&
&&  The purpose of this code is to calculate the EAN-13 barcode
&&
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
&&  Check to make sure data is numeric and remove dashes, etc.
    m.lcOnlyCorrectData = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& Remove check digits if they added one
    do case
        case Len(m.lcOnlyCorrectData) = 13
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 12)
        case Len(m.lcOnlyCorrectData) = 15
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 12) + SubStr(m.lcOnlyCorrectData, 14, 2)
        case Len(m.lcOnlyCorrectData) = 18
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 12) + SubStr(m.lcOnlyCorrectData, 14, 5)
    endcase
    m.lcEAN2AddOn = ""
    m.lcEAN5AddOn  = ""
    m.lcEANAddOnToPrint = ""
    do case
        case Len(m.lcOnlyCorrectData) = 14
            m.lcEAN2AddOn = SubStr(m.lcOnlyCorrectData, 13, 2)
        case Len(m.lcOnlyCorrectData) = 17
            m.lcEAN5AddOn = SubStr(m.lcOnlyCorrectData, 13, 5)
    endcase
&& split 12 digit number from add-on
    m.pcDataToEncode = left(m.lcOnlyCorrectData, 12)
&& <<<< Calculate Check Digit >>>>
    m.lnFactor = 3
    m.lnweightedTotal = 0
    For m.I = Len(m.pcDataToEncode) To 1 Step -1
&& Get the value of each number starting at the end
        m.lnCurrentCharNum = SubStr(m.pcDataToEncode, m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,1,3,1...
&& and add the sum together
        m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
        m.lnFactor = 4 - m.lnFactor
    Next m.I
&& Find the m.lnCheckDigit by finding the number + m.lnweightedTotal that = a multiple of 10
&& divide by 10, get the remainder and subtract from 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) % 10

&& Now we must encode the leading digit into the left half of the EAN-13 symbol
&& by using variable parity between character sets A and B
    m.lnLeadingDigit = val(left(m.pcDataToEncode, 1))
    do case
        Case m.lnLeadingDigit = 0
            m.lcEncoding = "AAAAAACCCCCC"
        Case m.lnLeadingDigit = 1
            m.lcEncoding = "AABABBCCCCCC"
        Case m.lnLeadingDigit = 2
            m.lcEncoding = "AABBABCCCCCC"
        Case m.lnLeadingDigit = 3
            m.lcEncoding = "AABBBACCCCCC"
        Case m.lnLeadingDigit = 4
            m.lcEncoding = "ABAABBCCCCCC"
        Case m.lnLeadingDigit = 5
            m.lcEncoding = "ABBAABCCCCCC"
        Case m.lnLeadingDigit = 6
            m.lcEncoding = "ABBBAACCCCCC"
        Case m.lnLeadingDigit = 7
            m.lcEncoding = "ABABABCCCCCC"
        Case m.lnLeadingDigit = 8
            m.lcEncoding = "ABABBACCCCCC"
        Case m.lnLeadingDigit = 9
            m.lcEncoding = "ABBABACCCCCC"
    endcase
&& add the check digit to the end of the barcode + remove the leading digit
    m.pcDataToEncode = SubStr(m.pcDataToEncode, 2, 11) + allt(str(m.lnCheckDigit))

&& Now that we have the total number including the check digit, determine character to print
&& for proper barcoding:

    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength
&& Get the ASCII value of each number excluding the first number because
&& it is encoded with variable parity
        m.lnCurrentCharNum = Asc(SubStr(m.pcDataToEncode, m.I, 1))
        m.lcCurrentEncoding = SubStr(m.lcEncoding, m.I, 1)
&& Print different barcodes according to the location of the m.lcCurrentChar and m.lcCurrentEncoding
        m.lcDataToPrint = m.lcDataToPrint + Chr(m.lnCurrentCharNum + ;
            iif(m.lcCurrentEncoding=="B", 17, iif(m.lcCurrentEncoding=="C", 27, 0) ) ) + ;
            iif(m.I=6,"*","")
&& add in the 1st character along with guard patterns
        if m.I=1
            m.lcDataToPrint = Chr((m.lnLeadingDigit + 48) + iif(m.lnLeadingDigit > 4,64,37) ) + "(" + m.lcDataToPrint
        endif
        if m.I=12
            m.lcDataToPrint = m.lcDataToPrint + "("
        endif
    Next m.I


&& Process 5 digit add on if it exists
    If Len(m.lcEAN5AddOn ) = 5
        m.lcEANAddOnToPrint = ""
&& Get check digit for add on
        m.lnFactor = 3
        m.lnweightedTotal = 0
        For m.I = 5 To 1 Step -1
&& Get the value of each number starting at the end
            m.lnCurrentCharNum = SubStr(m.lcEAN5AddOn , m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,9,3,9...
&& and add the sum together
            m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
            m.lnFactor = 12 - m.lnFactor
        Next m.I
&& Find the m.lnCheckDigit by extracting the right-most number from m.lnweightedTotal
        m.lnCheckDigit = m.lnweightedTotal % 10
&& Now we must encode the add-on m.lnCheckDigit into the number sets
&& by using variable parity between character sets A and B
        do case
            Case m.lnCheckDigit = 0
                m.lcEncoding = "BBAAA"
            Case m.lnCheckDigit = 1
                m.lcEncoding = "BABAA"
            Case m.lnCheckDigit = 2
                m.lcEncoding = "BAABA"
            Case m.lnCheckDigit = 3
                m.lcEncoding = "BAAAB"
            Case m.lnCheckDigit = 4
                m.lcEncoding = "ABBAA"
            Case m.lnCheckDigit = 5
                m.lcEncoding = "AABBA"
            Case m.lnCheckDigit = 6
                m.lcEncoding = "AAABB"
            Case m.lnCheckDigit = 7
                m.lcEncoding = "ABABA"
            Case m.lnCheckDigit = 8
                m.lcEncoding = "ABAAB"
            Case m.lnCheckDigit = 9
                m.lcEncoding = "AABAB"
        endcase
&& Now that we have the total number including the check digit, determine character to print
&& for proper barcoding:
        For m.I = 1 To 5
&& Get the value of each number
&& it is encoded with variable parity
            m.lcCurrentChar = SubStr(m.lcEAN5AddOn , m.I, 1)
            m.lcCurrentEncoding = SubStr(m.lcEncoding, m.I, 1)
&& Print different barcodes according to the location of the m.lcCurrentChar and m.lcCurrentEncoding
&& Note that here we use a converting table from digit to appropriate code
            m.lcEANAddOnToPrint = m.lcEANAddOnToPrint + ;
                chrtran(m.lcCurrentChar, NUMCHARS, ;
                iif(m.lcCurrentEncoding == "A", UPCaNUMTOCODE_A, UPCaNUMTOCODE_B ) )

&& add in the space + add-on guard pattern
            if m.I<5
&& Now print add-on delineators between each add-on character
                m.lcEANAddOnToPrint = iif(m.I=1,Chr(32) + Chr(43),"") + ;
                    m.lcEANAddOnToPrint + Chr(33)
            endif
        Next m.I
    endif

&& Process 2 digit add on if it exists
    If Len(m.lcEAN2AddOn) = 2 Then
        m.lcEANAddOnToPrint = ""
&& Get m.lcEncoding for add on
        m.lnAddOnNum = Val(m.lcEAN2AddOn) % 4
        m.lcEncoding = iif(m.lnAddOnNum>2,"B","A") + ;
            iif(m.lnAddOnNum % 2 = 1,"B","A")
&& Now that we have the total number including the m.lcEncoding
&& determine what to print
&& Get the value of each number
&& it is encoded with variable parity -- there are only 2 characters here

&& Print different barcodes according to the location of the m.lcCurrentChar and m.lcCurrentEncoding
&& Note that here we use a converting table from digit to appropriate code
&& add in the space + add-on guard pattern
        m.lcEANAddOnToPrint = ;
            Chr(32) + Chr(43) + ;
            chrtran(left(m.lcEAN2AddOn, 1), NUMCHARS, ;
            iif(left(m.lcEncoding, 1) == "A", UPCaNUMTOCODE_A, UPCaNUMTOCODE_B ) ) + ;
            Chr(33) + ;
            chrtran(right(m.lcEAN2AddOn, 1), NUMCHARS, ;
            iif(right(m.lcEncoding, 1) == "A", UPCaNUMTOCODE_A, UPCaNUMTOCODE_B ) )
    endif
&& Get Printable String
    m.lcPrintable_string = m.lcDataToPrint + m.lcEANAddOnToPrint + " "
&& Return PrintableString
    return m.lcPrintable_string
endproc

procedure EAN8
    lparameters pcDataToEncode
&&
&&  Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com or http://www.IDautomation.com
&&
&&  You may use our source code in your applications only if you are using barcode fonts created by IDautomation.com, Inc.
&&  and you do not remove the copyright notices in the source code.
&&
&&  The purpose of this code is to calculate the EAN-8 barcode
&&  Enter all the numbers without dashes
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
&&  Check to make sure data is numeric and remove dashes, etc.
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
    If Len(m.pcDataToEncode) <> 7 Then
        =Messagebox("Cannot process; you MUST enter a 7 digit NUMBER for this type of barcode. Do not use any spaces or dashes.")
        return
    endif
&& <<<< Calculate Check Digit >>>>
    m.lnFactor = 3
    m.lnweightedTotal = 0
    For m.I = Len(m.pcDataToEncode) To 1 Step -1
&& Get the value of each number starting at the end
        m.lnCurrentCharNum = SubStr(m.pcDataToEncode, m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,1,3,1...
&& and add the sum together
        m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
        m.lnFactor = 4 - m.lnFactor
    Next m.I
&& Find the m.lnCheckDigit by finding the number + m.lnweightedTotal that = a multiple of 10
&& divide by 10, get the remainder and subtract from 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) % 10

    m.pcDataToEncode = m.pcDataToEncode + allt(str(m.lnCheckDigit))

&& Now that have the total number including the check digit, determine character to print
&& for proper barcoding
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength
&& Get the ASCII value of each number
        m.lnCurrentCharNum = Asc(SubStr(m.pcDataToEncode, m.I, 1))
&& Print different barcodes according to the location of the m.lcCurrentChar and m.lcCurrentEncoding
&& Print different barcodes according to the location of the m.lcCurrentChar
        m.lcDataToPrint = iif(m.I = 1, "(", "") + ;
            m.lcDataToPrint + Chr(m.lnCurrentCharNum + ;
            iif(m.I > 4, 27, 0) ) + ;
            iif(m.I = 4, "*", "") + ;
            iif(m.I = 8, "(", "")
    Next m.I
&& Get Printable String
    m.lcPrintable_string = m.lcDataToPrint + " "
&& Display PrintableString in textbox
    return m.lcPrintable_string
endproc

procedure SSCC18
    lparameters pcDataToEncode, pnReturnType
&&  This code is Copyright, IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com
&&  The purpose for this code is to print a barcode
&&  according to the UCC/EAN SSCC-18 standards.

    local lcOnlyCorrectData, lnFactor, lnweightedTotal, lnStringLength, I, ;
        lnCurrentCharNum, lnCheckDigit
&&  Check to make sure data is numeric and remove dashes, etc.
    m.lcOnlyCorrectData = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& Remove check digits and (AI) if they were added to input
    if Len(m.lcOnlyCorrectData) > 18
        m.lcOnlyCorrectData = SubStr(m.lcOnlyCorrectData, 3, 17)
    else
        If Len(m.lcOnlyCorrectData) = 18
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 17)
        endif
    endif
&& End sub if incorrect number
    If Len(m.lcOnlyCorrectData) <> 17
        return
    endif
    m.pcDataToEncode = m.lcOnlyCorrectData
&& <<<< Generate MOD 10 check digit >>>>
    m.lnFactor = 3
    m.lnweightedTotal = 0
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = m.lnStringLength To 1 Step -1
&& Get the value of each number starting at the end
        m.lnCurrentCharNum = SubStr(m.pcDataToEncode, m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,1,3,1...
&& and add the sum together
        m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
        m.lnFactor = 4 - m.lnFactor
    Next m.I
&& Find the m.lnCheckDigit by finding the smallest number that = a multiple of 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) % 10
&& Add check digit and Application Identifier (AI) to m.pcDataToEncode
&& AI = 00 for SSCC18
&& m.pcDataToEncode = "00" + m.pcDataToEncode + m.lnCheckDigit
&& Now that we have calculated the MOD 10 for the data, send the string
&& to the UCC128() funtion. This function will:
&&  - Add in the Start C and m.lcFnc1 required by UCC/EAN
&&  - Calculate the MOD 103 required by UCC/EAN
&&  - Interleave the numbers into printable characters
&& m.pnReturnType 0 returns data formatted to the barcode font
    If m.pnReturnType = 0
        return UCC128("00" + m.pcDataToEncode + allt(str(m.lnCheckDigit)))
    endif
&& m.pnReturnType 1 returns data formatted for human readable text
    If m.pnReturnType = 1
        return "(00) " + left(m.pcDataToEncode, 1) + " " + ;
            SubStr(m.pcDataToEncode, 2, 7) + " " + ;
            SubStr(m.pcDataToEncode, 9, 9) + " " + allt(str(m.lnCheckDigit))
    endif
&& m.pnReturnType 2 returns the MOD10 check digit for the data supplied
    If m.pnReturnType = 2
        return Str(m.lnCheckDigit,2,0)
    endif
endproc

procedure SCC14
    lparameters pcDataToEncode, pnReturnType
&&  This code is Copyright, IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com
&&  The purpose for this code is to print a barcode
&&  according to the UCC/EAN SCC-14 standards.

    local lcOnlyCorrectData, lnFactor, lnweightedTotal, lnStringLength, I, ;
        lnCurrentCharNum, lnCheckDigit
&&  Check to make sure data is numeric and remove dashes, etc.
    m.lcOnlyCorrectData = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& Remove check digits and (AI) if they were added to input
    if Len(m.lcOnlyCorrectData) > 14
        m.lcOnlyCorrectData = SubStr(m.lcOnlyCorrectData, 3, 13)
    else
        If Len(m.lcOnlyCorrectData) = 14
            m.lcOnlyCorrectData = left(m.lcOnlyCorrectData, 13)
        endif
    endif
&& End sub if incorrect number
    If Len(m.lcOnlyCorrectData) <> 13
        return
    endif
    m.pcDataToEncode = m.lcOnlyCorrectData
&& <<<< Generate MOD 10 check digit >>>>
    m.lnFactor = 3
    m.lnweightedTotal = 0
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = m.lnStringLength To 1 Step -1
&& Get the value of each number starting at the end
        m.lnCurrentCharNum = SubStr(m.pcDataToEncode, m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,1,3,1...
&& and add the sum together
        m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
        m.lnFactor = 4 - m.lnFactor
    Next m.I
&& Find the m.lnCheckDigit by finding the smallest number that = a multiple of 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) % 10
&& Add check digit and Application Identifier (AI) to m.pcDataToEncode
&& AI = 00 for SSCC18
&& m.pcDataToEncode = "00" + m.pcDataToEncode + m.lnCheckDigit
&& Now that we have calculated the MOD 10 for the data, send the string
&& to the UCC128() funtion. This function will:
&&  - Add in the Start C and m.lcFnc1 required by UCC/EAN
&&  - Calculate the MOD 103 required by UCC/EAN
&&  - Interleave the numbers into printable characters
&& m.pnReturnType 0 returns data formatted to the barcode font
    If m.pnReturnType = 0
        return UCC128("01" + m.pcDataToEncode + allt(str(m.lnCheckDigit)))
    endif
&& m.pnReturnType 1 returns data formatted for human readable text
    If m.pnReturnType = 1
        return "(01) " + left(m.pcDataToEncode, 1) + " " + ;
            SubStr(m.pcDataToEncode, 2, 7) + " " + ;
            SubStr(m.pcDataToEncode, 9, 5) + " " + allt(str(m.lnCheckDigit))
    endif
&& m.pnReturnType 2 returns the MOD10 check digit for the data supplied
    If m.pnReturnType = 2
        Str(m.lnCheckDigit,2,0)
    endif
endproc

procedure UCC128
    lparameters pcDataToEncode
&&
&&  This code is Copyright, IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com
&&  The purpose for this code is to generate a check digit and print a barcode
&&  according to the UCC-128 EAN-128, SSCC-18 and SCC-14 standards.
&&
&&  UCC/EAN-128 calls for the m.lcFnc1 character to be entered, since this cannot
&&  be printed from the keyboard you must enter FA for the m.lcFnc1 code.
&&  The first m.lcFnc1 code is included automatically but you may need to enter this FA
&&  code if you need to enter another m.lcFnc1 code in the middle of the number.
&&  If you do this MAKE SURE that EVEN numbers are between "FA"; this code performs
&&  no checking for this!!
&&
&&  Here is an example:  1234FA567800
&&
&&  You MUST use the fully functional Code 128 (dated 12/2000 or later)
&&  font for this code to create and print a proper barcode
&&
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
&&  Check to make sure data is numeric or "FA" and remove all others.
    m.lcOnlyCorrectData = ""
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength Step 2
&&Add all numbers and "FA" to OnlyCorrectData string
        m.lcChars = SubStr(m.pcDataToEncode, I, 2)
        If IsNumber(m.lcChars)
            m.lcOnlyCorrectData = m.lcOnlyCorrectData + SubStr(m.pcDataToEncode, m.I, 2)
        endif
        If m.lcChars == "FA"
            m.lcOnlyCorrectData = m.lcOnlyCorrectData + SubStr(m.pcDataToEncode, m.I, 2)
        endif
    Next m.I
    m.pcDataToEncode = m.lcOnlyCorrectData
&& Assign start, stop and m.lcFnc1 codes
    m.lcStartCode = Chr(205)
    m.lcStopCode = Chr(206)
    m.lcFnc1 = Chr(202)
&&  m.lnCurrentValue
&& <<<< Calculate Modulo 103 Check Digit and generate m.lcDataToPrint >>>>
&& Set m.lnweightedTotal to the Code 128 value of the start character + m.lcFnc1
    m.lnweightedTotal = 105 + 102
    m.lnWeightValue = 2
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength Step 2
&& Get the value of each number pair
        m.lcCurrentChar = SubStr(m.pcDataToEncode, m.I, 2)
&& get the m.lcDataToPrint
        If m.lcCurrentChar == "FA" Then
            m.lcDataToPrint = m.lcDataToPrint + Chr(202)
&& multiply by the weighting character
            m.lnCurrentValue = 102 * m.lnWeightValue
        else
&& set the Integer m.lnCurrentValue to the number of String m.lcCurrentChar
            m.lnCurrentValue = val(m.lcCurrentChar)
            m.lcDataToPrint = m.lcDataToPrint + Chr(iif(m.lnCurrentValue = 0, 194, ;
                iif(m.lnCurrentValue > 94,m.lnCurrentValue + 100, m.lnCurrentValue + 32) ))
&& multiply by the weighting character
            m.lnCurrentValue = m.lnCurrentValue * m.lnWeightValue
        endif
&& add the values together to get the weighted total
        m.lnweightedTotal = m.lnweightedTotal + m.lnCurrentValue
        m.lnWeightValue = m.lnWeightValue + 1
    Next m.I
&& divide the m.lnweightedTotal by 103 and get the remainder, this is the m.lnCheckDigitValue
    m.lnCheckDigitValue = m.lnweightedTotal % 103
&& Now that we have the m.lnCheckDigitValue, find the corresponding ASCII character from the table
    m.lcC128_CheckDigit = Chr(iif(m.lnCheckDigitValue = 0,194, ;
        iif(m.lnCheckDigitValue > 94, m.lnCheckDigitValue + 100, m.lnCheckDigitValue + 32) ))

    m.lcPrintable_string = m.lcStartCode + m.lcFnc1 + m.lcDataToPrint + m.lcC128_CheckDigit + m.lcStopCode + " "
&& Return PrintableString
    return m.lcPrintable_string
endproc

procedure Code11
    lparameters pcDataToEncode
&&
&&  Copyright é IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com or http://www.IDautomation.com
&&
&&  You may use our source code in your applications only if you are using barcode fonts created by IDautomation.com, Inc.
&&  and you do not remove the copyright notices in the source code.
&&
&&  The purpose of this code is to calculate the Code 11 barcode
&&  Enter all the numbers without dashes
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)
&&  Check to make sure data is numeric or a dash and remove all others.
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS+"-", ""), "")
&& <<<< Calculate Check Digit >>>>
    m.lnFactor = 1
    m.lnweightedTotal = 0
    For m.I = Len(m.pcDataToEncode) To 1 Step -1
&& Get the value of each number starting at the end
        m.lcCurrentChar = SubStr(m.pcDataToEncode, m.I, 1)
&& Set the "-" character to the value of 10
        If m.lcCurrentChar = "-"
            m.lcCurrentChar = "10"
        endif
&& multiply by the weighting character and add together
        m.lnweightedTotal = m.lnweightedTotal + Val(m.lcCurrentChar) * m.lnFactor
&& change m.lnFactor for next calculation
        m.lnFactor = m.lnFactor + 1
    Next m.I
&& Find the Modulo 11 check digit
    m.lnCheckDigit = m.lnweightedTotal % 11
&& Get Printable String
    m.lcPrintable_string = "(" + m.pcDataToEncode + allt(str(m.lnCheckDigit)) + ")" + " "
&& Return the PrintableString
    return m.lcPrintable_string
endproc

procedure RM4SCC
    lparameters pcDataToEncode
&&
&&  This module is Copyright, IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com
&&
&&  The purpose of this code is to print the CODE 39 barcode
&&  You MUST install the AdvRM font for this application to print
&&
&&  Get data from user, this is the m.pcDataToEncode
    m.pcDataToEncode = Upper(alltrim(m.pcDataToEncode))
&& only pass correct data
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS+ALPHACHARS, ""), "")

    m.lcDataToPrint = m.pcDataToEncode

    m.lnRtotal = 0
    m.lnCtotal = 0

    m.lnweightedTotal = 0
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = 1 To m.lnStringLength
&& Get each character one at a time
        m.lcCurrentChar = SubStr(m.pcDataToEncode, m.I, 1)
&& Get the values of m.lcCurrentChar
&& Note that we use here a table to convert a character to code, then split that code to 2 digits
        m.lcVal= padl(asc(chrtran(m.lcCurrentChar, CHARS_RM4SCC, CODES_RM4SCC_RplusC)),2,"0")
        m.r = left(m.lcVal, 1)
        m.c = right(m.lcVal, 1)
&& add the values together
        m.lnRtotal = m.lnRtotal + val(m.r)
        m.lnCtotal = m.lnCtotal + val(m.c)
    Next m.I

&& divide the Totals by 6 and get the remainder, this is a reference
&& to the Check Digit.
&& set check digit to m.lcCurrentChar (a string)
    m.lnRtotal = m.lnRtotal % 6
    m.lnCtotal = m.lnCtotal % 6
&& note that here we use the same tbae to do a back converting from R+C codes to
&& correct character
    m.lcVal = val(allt(str(m.lnRtotal)) + allt(str(m.lnCtotal)) )
    m.lcCurrentChar = chrtran(chr(m.lcVal), CODES_RM4SCC_RplusC, CHARS_RM4SCC)
&& Get Printable String
    m.lcPrintable_string = "(" + m.lcDataToPrint + m.lcCurrentChar + ")" + " "
&& Return PrintableString
    return m.lcPrintable_string
endproc

procedure Codabar
    lparameters pcDataToEncode
&&
&&  This module is Copyright, IDautomation.com, Inc. 2001.  All rights reserved.
&&  For more info visit http://www.BizFonts.com
&&
&&  The purpose of this code is to print the Codabar barcode
    m.lcDataToPrint = ""
    m.pcDataToEncode = alltrim(m.pcDataToEncode)

&&  Check to make sure data is numeric, $, +, -, /, or :, and remove all others.
    m.lcDataToPrint = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, CHARSCodabar, ""), "")
&& Get Printable String
    m.lcPrintable_string = "A" + m.lcDataToPrint + "B" + " "
&& Return PrintableString
    return m.lcPrintable_string
endproc


procedure MOD10
    lparameters pcDataToEncode
&&  This is a general MOD10 function like the one required for EAN and UPC
&&  Check to make sure data is numeric and remove dashes, etc.
    m.pcDataToEncode = chrtran(m.pcDataToEncode, ;
        chrtran(m.pcDataToEncode, NUMCHARS, ""), "")
&& <<<< Generate MOD 10 check digit >>>>
    m.lnFactor = 3
    m.lnweightedTotal = 0
    m.lnStringLength = Len(m.pcDataToEncode)
    For m.I = m.lnStringLength To 1 Step -1
&& Get the value of each number starting at the end
        m.lnCurrentCharNum = SubStr(m.pcDataToEncode, m.I, 1)
&& multiply by the weighting m.lnFactor which is 3,1,3,1...
&& and add the sum together
        m.lnweightedTotal = m.lnweightedTotal + val(m.lnCurrentCharNum) * m.lnFactor
&& change m.lnFactor for next calculation
        m.lnFactor = 4 - m.lnFactor
    Next m.I
&& Find the m.lnCheckDigit by finding the smallest number that = a multiple of 10
    m.lnCheckDigit = (10 - (m.lnweightedTotal % 10)) % 10
    return Str(m.lnCheckDigit,2,0)
endproc

* simple function to check if character string contains number, include number with leading zeros
procedure IsNumber
    lparameters pcStr
    return !empty(m.pcStr) AND padl(allt(str(val(allt(m.pcStr)))),len(allt(m.pcStr)),"0")==allt(m.pcStr)


    #UNDEFINE NUMCHARS
    #UNDEFINE ALPHACHARS
    #UNDEFINE UPCaNUMTOCODE_A
    #UNDEFINE UPCaNUMTOCODE_B
    #UNDEFINE CHARSCode39Mod43
    #UNDEFINE CHARSCodabar
    #UNDEFINE CHARS_RM4SCC
    #UNDEFINE CODES_RM4SCC_RplusC
