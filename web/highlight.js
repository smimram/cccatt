(async ({chrome, netscape}) => {
  if (!chrome && !netscape) await import('https://unpkg.com/@ungap/custom-elements');
  const {default: HighlightedCode} = await import('https://unpkg.com/highlighted-code');
  const hljs = HighlightedCode.library;
  hljs.registerLanguage('cccatt', function() {
    return {
      case_insensitive: false,
      keywords: {
        $pattern: /[->a-z:*=×→.^?]+/,
        keyword: ['defined','coh','=',':='],
        operator: ['->','→','=>','*','×',':','.','=^\.^=','=\?\.\?=']
      },
      contains: [
        {
          className: 'string',
          begin: '"',
          end: '"'
        },
        hljs.COMMENT('#', '$', {})
      ]
    }
  })
  // https://github.com/highlightjs/highlight.js/tree/main/src/styles
  HighlightedCode.useTheme('github');
})(self);
