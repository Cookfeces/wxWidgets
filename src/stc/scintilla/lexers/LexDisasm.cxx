// Scintilla source code edit control
/** @file LexDisassemble.cxx
 ** Lexer for Disassemble, just for the AT&T syntax, for csky architecture
 ** Written by The Quxm
 **/
// Copyright 1998-2003 by Neil Hodgson <neilh@scintilla.org>
// The License.txt file describes the conditions under which this software may be distributed.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>

#include <string>
#include <map>
#include <set>

#include "ILexer.h"
#include "Scintilla.h"
#include "SciLexer.h"

#include "WordList.h"
#include "LexAccessor.h"
#include "StyleContext.h"
#include "CharacterSet.h"
#include "LexerModule.h"
#include "OptionSet.h"

#ifdef SCI_NAMESPACE
using namespace Scintilla;
#endif

static inline bool IsAWordChar(const int ch) {
	return (ch < 0x80) && (isalnum(ch) || ch == '.' ||
		ch == '_' || ch == '?');
}

static inline bool IsAWordStart(const int ch) {
	return (ch < 0x80) && (isalnum(ch) || ch == '_' || ch == '.' ||
		ch == '%' || ch == '@' || ch == '$' || ch == '?');
}

static bool IsSpaceEquiv(int state) {
	return (state == SCE_CSKY_COMMENT) ||
		   (state == SCE_CSKY_COMMENTLINE);
}

static inline bool IsAsmOperator(const int ch) {
	if ((ch < 0x80) && (isalnum(ch)))
		return false;
	// '.' left out as it is used to make up numbers
	if (ch == '*' || ch == '/' || ch == '-' || ch == '+' ||
		ch == '(' || ch == ')' || ch == '=' || ch == '^' ||
		ch == '[' || ch == ']' || ch == '<' || ch == '&' ||
		ch == '>' || ch == ',' || ch == '|' || ch == '~' ||
		ch == '%' || ch == ':')
		return true;
	return false;
}

static inline bool IsAHexChar(const int ch) {
	if (strchr("ABCDEFabcdef", ch) != NULL
		|| (isascii(ch) && isdigit(ch))) {
		return true;
	}

	return false;
}

static inline bool IsAHexCharacter(const int ch) {
	if (strchr("ABCDEFabcdef", ch) != NULL) {
		return true;
	}

	return false;
}

static const char * const cskyWordListDesc[] = {
	"CPU instructions",
	"FPU instructions",
	"Registers",
	"Directives",
	"Directive operands",
	"Extended instructions",
	0
};

class LexerDisasm : public ILexer {
	char *m_wordListSets;
	WordList cpuInstruction;
	WordList mathInstruction;
	WordList registers;
	WordList directive;
	WordList directiveOperand;
	WordList extInstruction;

	void SCI_METHOD InitWordListSets(void);
public:
	LexerDisasm(void);
	virtual ~LexerDisasm(void);
	void SCI_METHOD Release() {
		delete this;
	}
	int SCI_METHOD Version() const {
		return lvOriginal;
	}
	const char * SCI_METHOD PropertyNames() {
		return NULL;
	}
	int SCI_METHOD PropertyType(const char *) {
		return -1;
	}
	const char * SCI_METHOD DescribeProperty(const char *) {
		return NULL;
	}
	
	int SCI_METHOD PropertySet(const char *, const char *) {
		return -1;
	}
	const char * SCI_METHOD DescribeWordListSets();
	int SCI_METHOD WordListSet(int n, const char *wl);
	void SCI_METHOD Lex(unsigned int startPos, int length, int initStyle, IDocument *pAccess);
	void * SCI_METHOD PrivateCall(int, void *) {
		return 0;
	}
	void SCI_METHOD Fold(unsigned int startPos, int lengthDoc, int initStyle, IDocument *pAccess) {}

	static ILexer *LexerFactoryDisasm() {
		return new LexerDisasm();
	}
};

LexerDisasm::LexerDisasm(void) {
	this->InitWordListSets();
}

LexerDisasm::~LexerDisasm(void) {
	delete[] this->m_wordListSets;
}

void SCI_METHOD LexerDisasm::InitWordListSets(void)
{
	size_t totalLen = 0;


	for (int i=0; cskyWordListDesc[i]; i++) {
		totalLen += strlen(cskyWordListDesc[i]);
		totalLen++;
	};

	totalLen++;
	this->m_wordListSets = new char[totalLen];
	memset(this->m_wordListSets, 0, totalLen);

	for (int i=0; cskyWordListDesc[i]; i++) {
		strcat(this->m_wordListSets, cskyWordListDesc[i]);
		strcat(this->m_wordListSets, "\n");
	};
}

const char * SCI_METHOD LexerDisasm::DescribeWordListSets()
{
	return this->m_wordListSets;
}

int SCI_METHOD LexerDisasm::WordListSet(int n, const char *wl) {
	WordList *wordListN = 0;
	switch (n) {
	case 0:
		wordListN = &cpuInstruction;
		break;
	case 1:
		wordListN = &mathInstruction;
		break;
	case 2:
		wordListN = &registers;
		break;
	case 3:
		wordListN = &directive;
		break;
	case 4:
		wordListN = &directiveOperand;
		break;
	case 5:
		wordListN = &extInstruction;
		break;
	}
	int firstModification = -1;
	if (wordListN) {
		WordList wlNew;
		wlNew.Set(wl);
		if (*wordListN != wlNew) {
			wordListN->Set(wl);
			firstModification = 0;
		}
	}
	return firstModification;
}

void SCI_METHOD LexerDisasm::Lex(unsigned int startPos, int length, int initStyle, IDocument *pAccess) {
	LexAccessor styler(pAccess);
	int visibleChars = 0;
	bool isIncludePreprocessor = false;

	// Do not leak onto next line
	if (initStyle == SCE_DISASM_STRINGEOL)
		initStyle = SCE_DISASM_DEFAULT;

	StyleContext sc(startPos, length, initStyle, styler);

	for (; sc.More(); sc.Forward())
	{
		if (sc.atLineStart){
			// Prevent SCE_ASM_STRINGEOL from leaking back to previous line
			if (sc.state == SCE_DISASM_STRING) {
				sc.SetState(SCE_DISASM_STRING);
			} else if (sc.state == SCE_DISASM_CHARACTER) {
				sc.SetState(SCE_DISASM_CHARACTER);
			} else if (sc.state == SCE_DISASM_DEFAULT
			           || sc.state == SCE_DISASM_START) {
				sc.SetState(SCE_DISASM_START);
			}
			// Reset states to beginning of colourise so no surprises
			// if different sets of lines lexed.
			visibleChars = 0;
			isIncludePreprocessor = false;
		}

		// Handle line continuation generically.
		if (sc.ch == '\\') {
			if (sc.chNext == '\n' || sc.chNext == '\r') {
				sc.Forward();
				if (sc.ch == '\r' && sc.chNext == '\n') {
					sc.Forward();
				}
				continue;
			}
		}

		/* To handle special token combination in disassemble file.
		 * They are all at the begin of line.*/
		if (sc.state == SCE_DISASM_START) {
			while (IsASpaceOrTab(sc.ch)) {
				sc.ChangeState(SCE_DISASM_DEFAULT);
				sc.ForwardSetState(SCE_DISASM_START);
			}
			/* The combination in gdb disassemble:
			 *      0x------ stringA 
			 * if the stringA is consist if hex chars, and with
			 * space or tab followed by it, 0x----- is at the
			 * line begin, we recognize it as a number. */
			if (sc.Match('0', 'x') || sc.Match('0', 'X')) {
				sc.Forward(2);
				while (IsAHexChar(sc.ch)) {
					sc.Forward();
				}
				sc.ChangeState(SCE_DISASM_NUMBER);
				/* Since char turn to space when at line end,
				 * here must avoid the situation when the char
				 * is at the end of line. */
				if (!sc.atLineEnd && IsASpaceOrTab(sc.ch)) {
					sc.SetState(SCE_DISASM_DEFAULT);
					while (!sc.atLineEnd && IsASpaceOrTab(sc.ch)) {
						sc.Forward();
					}
					sc.SetState(SCE_DISASM_DEFAULT);
					if (IsAHexCharacter(sc.ch)) {
						sc.ChangeState(SCE_DISASM_NUMBER);
						while (IsAHexChar(sc.ch)) {
							sc.Forward();
						}
						/* If there is no space or tab after string,
						 * We don't recognize it as number. */
						if (sc.atLineEnd || !IsASpaceOrTab(sc.ch))
							sc.ChangeState(SCE_DISASM_IDENTIFIER);
					}
					else if (!isdigit(sc.ch)) {
						sc.ChangeState(SCE_DISASM_IDENTIFIER);
					}
					else {
						// Handle it as default.
					}
				}
				else {
					// Handle it as normal string begin with '0x'.
				}
			}
			/* The combination in objdump disassemble:
			 *      stringB: stringA 
			 * if the stringA and stringB is consist if hex chars,
			 * and with pace or tab followed by stringA, the stringB
			 * is at line begin, we recognize stringA an stringB as
			 * numbers. */
			else if (IsAHexChar(sc.ch)) {
				if (isascii(sc.ch) && isdigit(sc.ch))
					sc.ChangeState(SCE_DISASM_NUMBER);
				while (IsAHexChar(sc.chNext)) {
							sc.Forward();
				}
				if (sc.chNext == ':') {
					char ss[4],st[4];
					ss[0] = sc.ch;
					ss[1] = ':';
					ss[2] = ' ';
					ss[3] = NULL;
					st[0] = sc.ch;
					st[1] = ':';
					st[2] = '\t';
					st[3] = NULL;

					if (!sc.Match(ss) && !sc.Match(st)) {
						if (sc.state == SCE_DISASM_START)
							sc.ChangeState(SCE_DISASM_IDENTIFIER);
					}
					else {
						sc.ChangeState(SCE_DISASM_NUMBER);
						sc.ForwardSetState(SCE_DISASM_DEFAULT);
						sc.Forward();
						if (!sc.atLineEnd && IsASpaceOrTab(sc.ch)) {
							sc.SetState(SCE_DISASM_DEFAULT);
							while (!sc.atLineEnd && IsASpaceOrTab(sc.ch)) {
								sc.Forward();
							}
							sc.SetState(SCE_DISASM_DEFAULT);
							if (IsAHexCharacter(sc.ch)) {
								sc.ChangeState(SCE_DISASM_NUMBER);
								while (IsAHexChar(sc.ch)) {
									sc.Forward();
								}
								/* If there is no space or tab after string,
							 	* We don't recognize it as number. */
								if (sc.atLineEnd || !IsASpaceOrTab(sc.ch))
									sc.ChangeState(SCE_DISASM_IDENTIFIER);
							}
							else if (!isdigit(sc.ch)) {
								sc.ChangeState(SCE_DISASM_IDENTIFIER);
							}
							else {
								// Handle it as default.
							}
						}
						else {
							// Handle it as default.
						}
					}
				}
				else {
					if (sc.state == SCE_DISASM_START)
						sc.ChangeState(SCE_DISASM_IDENTIFIER);
				}
			}
			else {
				sc.ChangeState(SCE_DISASM_DEFAULT);
			}
		}

		// Determine if the current state should terminate.
		if (sc.state == SCE_DISASM_OPERATOR) {
			if (!IsAsmOperator(sc.ch)) {
			    sc.SetState(SCE_DISASM_DEFAULT);
			}
		} else if (sc.state == SCE_DISASM_NUMBER) {
			if (!IsAWordChar(sc.ch)) {
				sc.SetState(SCE_DISASM_DEFAULT);
			}
		} else if (sc.state == SCE_DISASM_IDENTIFIER) {
			if (!IsAWordChar(sc.ch) ) {
				char s[100];
				sc.GetCurrentLowered(s, sizeof(s));

				if (cpuInstruction.InList(s)) {
					sc.ChangeState(SCE_DISASM_CPUINSTRUCTION);
				} else if (mathInstruction.InList(s)) {
					sc.ChangeState(SCE_DISASM_MATHINSTRUCTION);
				} else if (registers.InList(s)) {
					sc.ChangeState(SCE_DISASM_REGISTER);
				}  else if (directive.InList(s)) {
					sc.ChangeState(SCE_DISASM_DIRECTIVE);
				} else if (directiveOperand.InList(s)) {
					sc.ChangeState(SCE_DISASM_DIRECTIVEOPERAND);
				} else if (extInstruction.InList(s)) {
					sc.ChangeState(SCE_DISASM_EXTINSTRUCTION);
				}
				sc.SetState(SCE_DISASM_DEFAULT);
			}
		} 
		else if (sc.state == SCE_DISASM_COMMENT ) {
			if (sc.Match('*', '/')) {
				sc.Forward();
				sc.ForwardSetState(SCE_DISASM_DEFAULT);
			}
		} else if (sc.state == SCE_DISASM_COMMENTLINE) {
			if (sc.atLineEnd) {
				sc.SetState(SCE_DISASM_DEFAULT);
			}
		}
		else if (sc.state == SCE_DISASM_STRING) {
			if (sc.ch == '\\') {
				if (sc.chNext == '\"' || sc.chNext == '\'' || sc.chNext == '\\') {
					sc.Forward();
				}
			} else if (sc.ch == '\"') {
				sc.ForwardSetState(SCE_DISASM_DEFAULT);
			} else if (sc.atLineEnd) {
				sc.ChangeState(SCE_DISASM_STRINGEOL);
				sc.ForwardSetState(SCE_DISASM_DEFAULT);
			} else if (isIncludePreprocessor) {
				if (sc.ch == '>') {
					sc.ForwardSetState(SCE_DISASM_DEFAULT);
					isIncludePreprocessor = false;
				}
			}
		} else if (sc.state == SCE_DISASM_CHARACTER) {
			if (sc.ch == '\\') {
				if (sc.chNext == '\"' || sc.chNext == '\'' || sc.chNext == '\\') {
					sc.Forward();
				}
			} else if (sc.ch == '\'') {
				sc.ForwardSetState(SCE_DISASM_DEFAULT);
			} else if (sc.atLineEnd) {
				sc.ChangeState(SCE_DISASM_STRINGEOL);
				sc.ForwardSetState(SCE_DISASM_DEFAULT);
			}
		} else if (sc.state == SCE_DISASM_PREPROCESSOR) {
			if (IsASpace(sc.ch)) {
				sc.SetState(SCE_DISASM_DEFAULT);
			}
		}

		// Determine if a new state should be entered.
		if (sc.state == SCE_DISASM_DEFAULT) {
			if (sc.Match('/', '*')){
				sc.SetState(SCE_DISASM_COMMENT);
			} else if (sc.Match('/', '/')){
				sc.SetState(SCE_DISASM_COMMENTLINE);
			} else if (isascii(sc.ch) && (isdigit(sc.ch) || (sc.ch == '.' && isascii(sc.chNext) && isdigit(sc.chNext)))) {
				sc.SetState(SCE_DISASM_NUMBER);
			} else if (IsAWordStart(sc.ch)) {
				sc.SetState(SCE_DISASM_IDENTIFIER);
			} else if (sc.ch == '\"') {
				sc.SetState(SCE_DISASM_STRING);
				isIncludePreprocessor = false;	// ensure that '>' won't end the string
			} else if (sc.ch == '\'') {
				sc.SetState(SCE_DISASM_CHARACTER);
			} else if (isIncludePreprocessor && sc.ch == '<') {
				sc.SetState(SCE_DISASM_STRING);
			} else if (IsAsmOperator(sc.ch)) {
				sc.SetState(SCE_DISASM_OPERATOR);
			} else if (sc.ch == '#' && visibleChars == 0) {
				// Preprocessor commands are alone on their line
				sc.SetState(SCE_DISASM_PREPROCESSOR);
				// Skip whitespace between # and preprocessor word
				do {
					sc.Forward();
				} while ((sc.ch == ' ' || sc.ch == '\t') && sc.More());
				if (sc.atLineEnd) {
					sc.SetState(SCE_DISASM_DEFAULT);
				} else if (sc.Match("include")) {
					isIncludePreprocessor = true;
				}
			}
		}

		if (!IsASpace(sc.ch) && !IsSpaceEquiv(sc.state)) {
			visibleChars++;
		}
	}
	sc.Complete();
}

LexerModule lmDisasm(SCLEX_DISASM, LexerDisasm::LexerFactoryDisasm, "disassemble", cskyWordListDesc);
