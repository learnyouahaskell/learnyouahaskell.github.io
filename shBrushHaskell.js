/**
 * Haskell Brush for Code Syntax Highlighter Version 1.5.1
 * Version 0.1
 * Copyright (C) 2008 Cristiano Paris <cristiano.paris@gmail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, version 3 of the License.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

dp.sh.Brushes.Haskell = function()
{
        var _split = function(str)
                     {
                       return "\\s"+str.replace(/ /g,"\\s|\\s").replace(/>/g, '&gt;').replace(/</g, '&lt;')+"\\s";
                     }

        var keywords = 'do else if in infixl infixr let module case of ' + 
                       'primitive then import as hiding qualified default';
        
        var keywords2 = 'class where data deriving instance type newtype';

        var syntax_operators = '=> -> <- ::'; 

        var common_operators = "\\$ \\. >>= >> \\+ \\* \\+\\+ = \\| \\- <= >= < > /= == \\^ \\/ : \\<\\*\\> \\<\\$\\>";

        this.regexList = [
                { regex: /--.*$/gm, css: 'comments' },                      			// one line comments
                { regex: /{-[\s\S]*?-}/gm, css: 'comments' },                      	        // multiline comments
                { regex: dp.sh.RegexLib.DoubleQuotedString,  css: 'string' },   		// double quoted strings
                { regex: new RegExp('^ *#.*', 'gm'), css: 'preprocessor' },			// preprocessor
                { regex: new RegExp(this.GetKeywords(keywords), 'g'), css: 'keyword' },     	// keyword
                { regex: new RegExp(this.GetKeywords(keywords2), 'g'), css: 'keyword2' },     	// keyword
                { regex: new RegExp(_split(syntax_operators), 'g'), css: 'syntax_operators' },  // syntax operators
                { regex: new RegExp(_split(common_operators), 'g'), css: 'common_operators' },  // common operators
                { regex: /\b[0-9]+(\.[0-9]+)?\b/g, css: 'numbers' }, //numbers
                { regex: /'.'/g, css:'char'}, //chars
                { regex: /'\\\d+'/g, css:'char'},
                { regex: /ghci&gt;/g, css:'ghci'}, //ghci prompt
                { regex: /`(\w|')+`/g, css: 'common_operators' },  					// common operators
                { regex: /\b[A-Z]\w*\b/g, css: 'type_constructors' },  				// type constructors
                         ];

        this.CssClass = 'dp-hs';

        this.Style = '.dp-hs .syntax_operators { color: #8ac6f2; }' +
                     '.dp-hs .common_operators { color: #8ac6f2; }' +
                     '';
};

dp.sh.Brushes.Haskell.prototype     = new dp.sh.Highlighter();
dp.sh.Brushes.Haskell.Aliases       = ['haskell','hs'];
// Local Variables:
// mode: Haskell
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:

