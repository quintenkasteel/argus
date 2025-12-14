import React from 'react';
import ComponentCreator from '@docusaurus/ComponentCreator';

export default [
  {
    path: '/__docusaurus/debug',
    component: ComponentCreator('/__docusaurus/debug', '5ff'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/config',
    component: ComponentCreator('/__docusaurus/debug/config', '5ba'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/content',
    component: ComponentCreator('/__docusaurus/debug/content', 'a2b'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/globalData',
    component: ComponentCreator('/__docusaurus/debug/globalData', 'c3c'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/metadata',
    component: ComponentCreator('/__docusaurus/debug/metadata', '156'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/registry',
    component: ComponentCreator('/__docusaurus/debug/registry', '88c'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/routes',
    component: ComponentCreator('/__docusaurus/debug/routes', '000'),
    exact: true
  },
  {
    path: '/docs',
    component: ComponentCreator('/docs', 'b81'),
    routes: [
      {
        path: '/docs',
        component: ComponentCreator('/docs', 'adc'),
        routes: [
          {
            path: '/docs',
            component: ComponentCreator('/docs', 'ee0'),
            routes: [
              {
                path: '/docs/architecture/analysis-passes',
                component: ComponentCreator('/docs/architecture/analysis-passes', 'bf6'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/architecture/fix-system',
                component: ComponentCreator('/docs/architecture/fix-system', 'a6d'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/architecture/hie-integration',
                component: ComponentCreator('/docs/architecture/hie-integration', '4ae'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/architecture/overview',
                component: ComponentCreator('/docs/architecture/overview', 'dc8'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/architecture/plugin-mode',
                component: ComponentCreator('/docs/architecture/plugin-mode', '807'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/architecture/rule-engine',
                component: ComponentCreator('/docs/architecture/rule-engine', 'd57'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/cli-reference/',
                component: ComponentCreator('/docs/cli-reference/', '5e8'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/cli-reference/check',
                component: ComponentCreator('/docs/cli-reference/check', 'ba8'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/cli-reference/daemon',
                component: ComponentCreator('/docs/cli-reference/daemon', '219'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/cli-reference/fix',
                component: ComponentCreator('/docs/cli-reference/fix', '647'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/cli-reference/index-cmd',
                component: ComponentCreator('/docs/cli-reference/index-cmd', 'ddd'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/cli-reference/init',
                component: ComponentCreator('/docs/cli-reference/init', '999'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/cli-reference/lsp',
                component: ComponentCreator('/docs/cli-reference/lsp', '3a3'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/cli-reference/output-formats',
                component: ComponentCreator('/docs/cli-reference/output-formats', '573'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/cli-reference/unused',
                component: ComponentCreator('/docs/cli-reference/unused', 'e34'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/cli-reference/watch',
                component: ComponentCreator('/docs/cli-reference/watch', '37f'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/configuration/complexity-section',
                component: ComponentCreator('/docs/configuration/complexity-section', '386'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/configuration/extensions-section',
                component: ComponentCreator('/docs/configuration/extensions-section', '013'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/configuration/file-format',
                component: ComponentCreator('/docs/configuration/file-format', '04e'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/configuration/fix-section',
                component: ComponentCreator('/docs/configuration/fix-section', '3eb'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/configuration/naming-section',
                component: ComponentCreator('/docs/configuration/naming-section', '4be'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/configuration/output-section',
                component: ComponentCreator('/docs/configuration/output-section', 'dfc'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/configuration/rules-section',
                component: ComponentCreator('/docs/configuration/rules-section', '245'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/configuration/scopes-section',
                component: ComponentCreator('/docs/configuration/scopes-section', 'c55'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/contributing/',
                component: ComponentCreator('/docs/contributing/', '051'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/contributing/adding-rules',
                component: ComponentCreator('/docs/contributing/adding-rules', '8c9'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/contributing/code-style',
                component: ComponentCreator('/docs/contributing/code-style', '78f'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/contributing/development-setup',
                component: ComponentCreator('/docs/contributing/development-setup', '989'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/contributing/pull-requests',
                component: ComponentCreator('/docs/contributing/pull-requests', 'd55'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/contributing/testing-guide',
                component: ComponentCreator('/docs/contributing/testing-guide', '540'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/faq/',
                component: ComponentCreator('/docs/faq/', 'b20'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/faq/error-messages',
                component: ComponentCreator('/docs/faq/error-messages', '651'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/faq/general',
                component: ComponentCreator('/docs/faq/general', '9be'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/faq/troubleshooting',
                component: ComponentCreator('/docs/faq/troubleshooting', '54a'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/getting-started/basic-usage',
                component: ComponentCreator('/docs/getting-started/basic-usage', '62e'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/getting-started/configuration',
                component: ComponentCreator('/docs/getting-started/configuration', 'a00'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/getting-started/installation',
                component: ComponentCreator('/docs/getting-started/installation', 'c16'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/getting-started/project-setup',
                component: ComponentCreator('/docs/getting-started/project-setup', 'cd9'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/overview/introduction',
                component: ComponentCreator('/docs/overview/introduction', '9c2'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/overview/key-features',
                component: ComponentCreator('/docs/overview/key-features', 'd0c'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/overview/quickstart',
                component: ComponentCreator('/docs/overview/quickstart', '3ec'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/architecture',
                component: ComponentCreator('/docs/rules/architecture', '4fc'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/complexity',
                component: ComponentCreator('/docs/rules/complexity', '872'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/custom-rules',
                component: ComponentCreator('/docs/rules/custom-rules', '83e'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/imports',
                component: ComponentCreator('/docs/rules/imports', '7b3'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/modernize',
                component: ComponentCreator('/docs/rules/modernize', 'ab9'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/naming',
                component: ComponentCreator('/docs/rules/naming', '436'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/overview',
                component: ComponentCreator('/docs/rules/overview', 'de7'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/partial-functions',
                component: ComponentCreator('/docs/rules/partial-functions', 'ac6'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/performance',
                component: ComponentCreator('/docs/rules/performance', '9e2'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/pragmas',
                component: ComponentCreator('/docs/rules/pragmas', 'af2'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/redundant',
                component: ComponentCreator('/docs/rules/redundant', '17d'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/security',
                component: ComponentCreator('/docs/rules/security', '74b'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/rules/space-leaks',
                component: ComponentCreator('/docs/rules/space-leaks', 'a61'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/testing/golden-tests',
                component: ComponentCreator('/docs/testing/golden-tests', '4df'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/testing/running-tests',
                component: ComponentCreator('/docs/testing/running-tests', '5b4'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/testing/test-fixtures',
                component: ComponentCreator('/docs/testing/test-fixtures', '0f3'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/testing/writing-tests',
                component: ComponentCreator('/docs/testing/writing-tests', '19d'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/usage-guide/analysis-modes',
                component: ComponentCreator('/docs/usage-guide/analysis-modes', '28c'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/usage-guide/auto-fix',
                component: ComponentCreator('/docs/usage-guide/auto-fix', '745'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/usage-guide/ci-integration',
                component: ComponentCreator('/docs/usage-guide/ci-integration', '4b5'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/usage-guide/ide-integration',
                component: ComponentCreator('/docs/usage-guide/ide-integration', '51a'),
                exact: true,
                sidebar: "docsSidebar"
              },
              {
                path: '/docs/usage-guide/working-with-hie',
                component: ComponentCreator('/docs/usage-guide/working-with-hie', 'd84'),
                exact: true,
                sidebar: "docsSidebar"
              }
            ]
          }
        ]
      }
    ]
  },
  {
    path: '/',
    component: ComponentCreator('/', 'e5f'),
    exact: true
  },
  {
    path: '*',
    component: ComponentCreator('*'),
  },
];
