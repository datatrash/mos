const { description } = require('../../package')

module.exports = {
  /**
   * Ref：https://v1.vuepress.vuejs.org/config/#title
   */
  title: 'MOS',
  /**
   * Ref：https://v1.vuepress.vuejs.org/config/#description
   */
  description: description,

  /**
   * Extra tags to be injected to the page HTML `<head>`
   *
   * ref：https://v1.vuepress.vuejs.org/config/#head
   */
  head: [
    ['meta', { name: 'theme-color', content: '#af3e3e' }],
    ['meta', { name: 'apple-mobile-web-app-capable', content: 'yes' }],
    ['meta', { name: 'apple-mobile-web-app-status-bar-style', content: 'black' }]
  ],

  /**
   * Theme configuration, here is the default theme configuration for VuePress.
   *
   * ref：https://v1.vuepress.vuejs.org/theme/default-theme-config.html
   */
  themeConfig: {
    repo: 'https://github.com/datatrash/mos',
    editLinks: false,
    docsDir: 'docs/src',
    editLinkText: '',
    lastUpdated: false,
    nav: [
      {
        text: 'Guide',
        link: '/guide/',
      }
    ],
    sidebar: {
      '/guide/': [
        {
          title: 'Getting started',
          collapsable: false,
          children: [
            '',
            'project-setup',
            'assembler',
            'advanced',
            'unit-testing'
          ]
        },
        {
          title: 'IDE support',
          collapsable: false,
          children: [
            'ide/',
            'ide/vscode'
          ]
        },
        {
          title: 'CLI',
          collapsable: false,
          children: [
            'cli/'
          ]
        }
      ],
    }
  },

  markdown: {
    lineNumbers: true
  },

  /**
   * Apply plugins，ref：https://v1.vuepress.vuejs.org/zh/plugin/
   */
  plugins: [
    '@vuepress/plugin-back-to-top',
    '@vuepress/plugin-medium-zoom',
    '@vuepress/active-header-links',
    [ 'sitemap', { hostname: 'http://mos.datatra.sh' } ]
  ]
}
