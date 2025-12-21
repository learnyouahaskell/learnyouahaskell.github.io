dp.sh.Brushes.Plain = function() {
    var common_operators = '$';
    var _split = function(str)
                 {
                   return "\\s"+str.replace(/ /g,"\\s|\\s").replace(/>/g, '&gt;').replace(/</g, '&lt;')+"\\s";
                 }
    this.regexList = [
                { regex: new RegExp('^ *#.*', 'gm'), css: 'preprocessor' },			// preprocessor
                { regex: new RegExp(_split(common_operators), 'g'), css: 'common_operators' }]  // common operators
    this.CssClass = 'dp-hs';
}
dp.sh.Brushes.Plain.prototype = new dp.sh.Highlighter();
dp.sh.Brushes.Plain.Aliases = ['plain'];
