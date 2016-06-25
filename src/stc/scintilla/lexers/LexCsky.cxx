// Scintilla source code edit control
/** @file LexCsky.cxx
 ** Lexer for Assembler, just for the AT&T syntax, for csky architecture
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

bool IsSpaceEquiv(int state) {
	return (state == SCE_CSKY_COMMENT) ||
		   (state == SCE_CSKY_COMMENTLINE);
}

static bool IsStreamCommentStyle(int style) {
	return (style == SCE_CSKY_COMMENT);
}

static inline int LowerCase(int c) {
	if (c >= 'A' && c <= 'Z')
		return 'a' + c - 'A';
	return c;
}

// An individual named option for use in an OptionSet

// Options used for LexerAsm
struct OptionsCsky {
	bool fold;
	bool foldSyntaxBased;
	bool foldComment;
	bool foldCommentMultiline;
	bool foldCommentExplicit;
	std::string foldExplicitStart;
	std::string foldExplicitEnd;
	bool foldExplicitAnywhere;
	bool foldCompact;
	bool trackPreprocessor;
	OptionsCsky() {
		fold = false;
		foldSyntaxBased = true;
		foldComment = false;
		foldCommentMultiline = true;
		foldCommentExplicit = true;
		foldExplicitStart = "";
		foldExplicitEnd   = "";
		foldExplicitAnywhere = false;
		foldCompact = true;
		trackPreprocessor = true;
	}
};

static const char * const cskyWordListDesc[] = {
	"CPU instructions",
	"FPU instructions",
	"Registers",
	"Directives",
	"Directive operands",
	"Extended instructions",
	"Directives4Foldstart",
	"Directives4Foldend",
	0
};

struct OptionSetCsky : public OptionSet<OptionsCsky> {
	OptionSetCsky() {
		DefineProperty("fold", &OptionsCsky::fold);
		
		DefineProperty("lexer.csky.track.preprocessor", &OptionsCsky::trackPreprocessor,
			"Set to 1 to interpret #if/#else/#endif to grey out code that is not active.");

		DefineProperty("fold.csky.syntax.based", &OptionsCsky::foldSyntaxBased,
			"Set this property to 0 to disable syntax based folding.");

		DefineProperty("fold.comment", &OptionsCsky::foldComment,
			"This option enables folding multi-line comments and explicit fold points when using the C++ lexer. "
			"Explicit fold points allows adding extra folding by placing a //{ comment at the start and a //} "
			"at the end of a section that should fold.");

		DefineProperty("fold.csky.comment.multiline", &OptionsCsky::foldCommentMultiline,
			"Set this property to 1 to enable folding multi-line comments.");

		DefineProperty("fold.csky.comment.explicit", &OptionsCsky::foldCommentExplicit,
			"This option enables folding explicit fold points when using the Asm lexer. "
			"Explicit fold points allows adding extra folding by placing a ;{ comment at the start and a ;} "
			"at the end of a section that should fold.");

		DefineProperty("fold.csky.explicit.start", &OptionsCsky::foldExplicitStart,
			"The string to use for explicit fold start points, replacing the standard ;{.");

		DefineProperty("fold.csky.explicit.end", &OptionsCsky::foldExplicitEnd,
			"The string to use for explicit fold end points, replacing the standard ;}.");

		DefineProperty("fold.csky.explicit.anywhere", &OptionsCsky::foldExplicitAnywhere,
			"Set this property to 1 to enable explicit fold points anywhere, not just in line comments.");

		DefineProperty("fold.compact", &OptionsCsky::foldCompact);

		DefineWordListSets(cskyWordListDesc);
	}
};

class LexerCsky : public ILexer {
	WordList cpuInstruction;
	WordList mathInstruction;
	WordList registers;
	WordList directive;
	WordList directiveOperand;
	WordList extInstruction;
	WordList directives4foldstart;
	WordList directives4foldend;
	OptionsCsky options;
	OptionSetCsky osAsm;
public:
	LexerCsky() {
	}
	virtual ~LexerCsky() {
	}
	void SCI_METHOD Release() {
		delete this;
	}
	int SCI_METHOD Version() const {
		return lvOriginal;
	}
	const char * SCI_METHOD PropertyNames() {
		return osAsm.PropertyNames();
	}
	int SCI_METHOD PropertyType(const char *name) {
		return osAsm.PropertyType(name);
	}
	const char * SCI_METHOD DescribeProperty(const char *name) {
		return osAsm.DescribeProperty(name);
	}
	int SCI_METHOD PropertySet(const char *key, const char *val);
	const char * SCI_METHOD DescribeWordListSets() {
		return osAsm.DescribeWordListSets();
	}
	int SCI_METHOD WordListSet(int n, const char *wl);
	void SCI_METHOD Lex(unsigned int startPos, int length, int initStyle, IDocument *pAccess);
	void SCI_METHOD Fold(unsigned int startPos, int length, int initStyle, IDocument *pAccess);

	void * SCI_METHOD PrivateCall(int, void *) {
		return 0;
	}

	static ILexer *LexerFactoryCsky() {
		return new LexerCsky();
	}
};

int SCI_METHOD LexerCsky::PropertySet(const char *key, const char *val) {
	if (osAsm.PropertySet(&options, key, val)) {
		return 0;
	}
	return -1;
}

int SCI_METHOD LexerCsky::WordListSet(int n, const char *wl) {
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
	case 6:
		wordListN = &directives4foldstart;
		break;
	case 7:
		wordListN = &directives4foldend;
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

void SCI_METHOD LexerCsky::Lex(unsigned int startPos, int length, int initStyle, IDocument *pAccess) {
	LexAccessor styler(pAccess);
	int visibleChars = 0;
	bool isIncludePreprocessor = false;

	// Do not leak onto next line
	if (initStyle == SCE_CSKY_STRINGEOL)
		initStyle = SCE_CSKY_DEFAULT;

	StyleContext sc(startPos, length, initStyle, styler);

	for (; sc.More(); sc.Forward())
	{
		if (sc.atLineStart){
			// Prevent SCE_ASM_STRINGEOL from leaking back to previous line
			if (sc.state == SCE_CSKY_STRING) {
				sc.SetState(SCE_CSKY_STRING);
			} else if (sc.state == SCE_CSKY_CHARACTER) {
				sc.SetState(SCE_CSKY_CHARACTER);
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

		// Determine if the current state should terminate.
		if (sc.state == SCE_CSKY_OPERATOR) {
			if (!IsAsmOperator(sc.ch)) {
			    sc.SetState(SCE_CSKY_DEFAULT);
			}
		} else if (sc.state == SCE_CSKY_NUMBER) {
			if (!IsAWordChar(sc.ch)) {
				sc.SetState(SCE_CSKY_DEFAULT);
			}
		} else if (sc.state == SCE_CSKY_IDENTIFIER) {
			if (!IsAWordChar(sc.ch) ) {
				char s[100];
				sc.GetCurrentLowered(s, sizeof(s));

				if (cpuInstruction.InList(s)) {
					sc.ChangeState(SCE_CSKY_CPUINSTRUCTION);
				} else if (mathInstruction.InList(s)) {
					sc.ChangeState(SCE_CSKY_MATHINSTRUCTION);
				} else if (registers.InList(s)) {
					sc.ChangeState(SCE_CSKY_REGISTER);
				}  else if (directive.InList(s)) {
					sc.ChangeState(SCE_CSKY_DIRECTIVE);
				} else if (directiveOperand.InList(s)) {
					sc.ChangeState(SCE_CSKY_DIRECTIVEOPERAND);
				} else if (extInstruction.InList(s)) {
					sc.ChangeState(SCE_CSKY_EXTINSTRUCTION);
				}
				sc.SetState(SCE_CSKY_DEFAULT);
			}
		} 
		else if (sc.state == SCE_CSKY_COMMENT ) {
			if (sc.Match('*', '/')) {
				sc.Forward();
				sc.ForwardSetState(SCE_CSKY_DEFAULT);
			}
		} else if (sc.state == SCE_CSKY_COMMENTLINE) {
			if (sc.atLineEnd) {
				sc.SetState(SCE_CSKY_DEFAULT);
			}
		}
		else if (sc.state == SCE_CSKY_STRING) {
			if (sc.ch == '\\') {
				if (sc.chNext == '\"' || sc.chNext == '\'' || sc.chNext == '\\') {
					sc.Forward();
				}
			} else if (sc.ch == '\"') {
				sc.ForwardSetState(SCE_CSKY_DEFAULT);
			} else if (sc.atLineEnd) {
				sc.ChangeState(SCE_CSKY_STRINGEOL);
				sc.ForwardSetState(SCE_CSKY_DEFAULT);
			} else if (isIncludePreprocessor) {
				if (sc.ch == '>') {
					sc.ForwardSetState(SCE_CSKY_DEFAULT);
					isIncludePreprocessor = false;
				}
			}
		} else if (sc.state == SCE_CSKY_CHARACTER) {
			if (sc.ch == '\\') {
				if (sc.chNext == '\"' || sc.chNext == '\'' || sc.chNext == '\\') {
					sc.Forward();
				}
			} else if (sc.ch == '\'') {
				sc.ForwardSetState(SCE_CSKY_DEFAULT);
			} else if (sc.atLineEnd) {
				sc.ChangeState(SCE_CSKY_STRINGEOL);
				sc.ForwardSetState(SCE_CSKY_DEFAULT);
			}
		} else if (sc.state == SCE_CSKY_PREPROCESSOR) {
			if (IsASpace(sc.ch)) {
				sc.SetState(SCE_CSKY_DEFAULT);
			}
		}

		// Determine if a new state should be entered.
		if (sc.state == SCE_CSKY_DEFAULT) {
			if (sc.Match('/', '*')){
				sc.SetState(SCE_CSKY_COMMENT);
			} else if (sc.Match('/', '/')){
				sc.SetState(SCE_CSKY_COMMENTLINE);
			} else if (isascii(sc.ch) && (isdigit(sc.ch) || (sc.ch == '.' && isascii(sc.chNext) && isdigit(sc.chNext)))) {
				sc.SetState(SCE_CSKY_NUMBER);
			} else if (IsAWordStart(sc.ch)) {
				sc.SetState(SCE_CSKY_IDENTIFIER);
			} else if (sc.ch == '\"') {
				sc.SetState(SCE_CSKY_STRING);
				isIncludePreprocessor = false;	// ensure that '>' won't end the string
			} else if (sc.ch == '\'') {
				sc.SetState(SCE_CSKY_CHARACTER);
			} else if (isIncludePreprocessor && sc.ch == '<') {
				sc.SetState(SCE_CSKY_STRING);
			} else if (IsAsmOperator(sc.ch)) {
				sc.SetState(SCE_CSKY_OPERATOR);
			} else if (sc.ch == '#' && visibleChars == 0) {
				// Preprocessor commands are alone on their line
				sc.SetState(SCE_CSKY_PREPROCESSOR);
				// Skip whitespace between # and preprocessor word
				do {
					sc.Forward();
				} while ((sc.ch == ' ' || sc.ch == '\t') && sc.More());
				if (sc.atLineEnd) {
					sc.SetState(SCE_CSKY_DEFAULT);
				} else if (sc.Match("include")) {
					isIncludePreprocessor = true;
				} else {
					if (options.trackPreprocessor) {
						if (!(sc.Match("ifdef") || sc.Match("ifndef") ||
							  sc.Match("if") || sc.Match("else") ||
							  sc.Match("elif") || sc.Match("endif") ||
							  sc.Match("define") || sc.Match("undef")))
							  sc.ChangeState(SCE_CSKY_DEFAULT);
					}
				}
			}
		}

		if (!IsASpace(sc.ch) && !IsSpaceEquiv(sc.state)) {
			visibleChars++;
		}
	}
	sc.Complete();
}

// Store both the current line's fold level and the next lines in the
// level store to make it easy to pick up with each increment
// and to make it possible to fiddle the current level for "else".

void SCI_METHOD LexerCsky::Fold(unsigned int startPos, int length, int initStyle, IDocument *pAccess) {

	if (!options.fold)
		return;

	LexAccessor styler(pAccess);

	unsigned int endPos = startPos + length;
	int visibleChars = 0;
	int lineCurrent = styler.GetLine(startPos);
	int levelCurrent = SC_FOLDLEVELBASE;
	if (lineCurrent > 0)
		levelCurrent = styler.LevelAt(lineCurrent-1) >> 16;
	int levelNext = levelCurrent;
	char chNext = styler[startPos];
	int styleNext = styler.StyleAt(startPos);
	int style = initStyle;
	char word[100];
	int wordlen = 0;
	const bool userDefinedFoldMarkers = !options.foldExplicitStart.empty() && !options.foldExplicitEnd.empty();
	for (unsigned int i = startPos; i < endPos; i++) {
		char ch = chNext;
		chNext = styler.SafeGetCharAt(i + 1);
		int stylePrev = style;
		style = styleNext;
		styleNext = styler.StyleAt(i + 1);
		bool atEOL = (ch == '\r' && chNext != '\n') || (ch == '\n');
		if (options.foldComment && options.foldCommentMultiline && 
			IsStreamCommentStyle(style)) {
			if (!IsStreamCommentStyle(stylePrev)) {
				levelNext++;
			} else if (!IsStreamCommentStyle(styleNext) && !atEOL) {
				// Comments don't end at end of line and the next character may be unstyled.
				levelNext--;
			}
		}
		if (options.foldComment && options.foldCommentExplicit &&
			((style == SCE_CSKY_COMMENTLINE) || options.foldExplicitAnywhere)) {
			if (userDefinedFoldMarkers) {
				if (styler.Match(i, options.foldExplicitStart.c_str())) {
 					levelNext++;
				} else if (styler.Match(i, options.foldExplicitEnd.c_str())) {
 					levelNext--;
 				}
			} else {
				if (ch == ';') {
					if (chNext == '{') {
						levelNext++;
					} else if (chNext == '}') {
						levelNext--;
					}
				}
 			}
 		}
		if (options.foldSyntaxBased && (style == SCE_CSKY_DIRECTIVE)) {
			word[wordlen++] = static_cast<char>(LowerCase(ch));
			if (wordlen == 100) {                   // prevent overflow
				word[0] = '\0';
				wordlen = 1;
			}
			if (styleNext != SCE_CSKY_DIRECTIVE) {   // reading directive ready
				word[wordlen] = '\0';
				wordlen = 0;
				if (directives4foldstart.InList(word)) {
					levelNext++;
				} else if (directives4foldend.InList(word)){
					levelNext--;
				}
			}
		}
		if (!IsASpace(ch))
			visibleChars++;
		if (atEOL || (i == endPos-1)) {
			int levelUse = levelCurrent;
			int lev = levelUse | levelNext << 16;
			if (visibleChars == 0 && options.foldCompact)
				lev |= SC_FOLDLEVELWHITEFLAG;
			if (levelUse < levelNext)
				lev |= SC_FOLDLEVELHEADERFLAG;
			if (lev != styler.LevelAt(lineCurrent)) {
				styler.SetLevel(lineCurrent, lev);
			}
			lineCurrent++;
			levelCurrent = levelNext;
			if (atEOL && (i == static_cast<unsigned int>(styler.Length()-1))) {
				// There is an empty line at end of file so give it same level and empty
				styler.SetLevel(lineCurrent, (levelCurrent | levelCurrent << 16) | SC_FOLDLEVELWHITEFLAG);
			}
			visibleChars = 0;
		}
	}
}

LexerModule lmCsky(SCLEX_CSKY, LexerCsky::LexerFactoryCsky, "csky", cskyWordListDesc);

