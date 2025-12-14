import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  docsSidebar: [
    {
      type: 'category',
      label: 'Overview',
      collapsed: false,
      items: [
        'overview/introduction',
        'overview/key-features',
        'overview/quickstart',
      ],
    },
    {
      type: 'category',
      label: 'Getting Started',
      collapsed: false,
      items: [
        'getting-started/installation',
        'getting-started/basic-usage',
        'getting-started/configuration',
        'getting-started/project-setup',
      ],
    },
    {
      type: 'category',
      label: 'Usage Guide',
      items: [
        'usage-guide/analysis-modes',
        'usage-guide/auto-fix',
        'usage-guide/ci-integration',
        'usage-guide/ide-integration',
        'usage-guide/working-with-hie',
      ],
    },
    {
      type: 'category',
      label: 'Rules',
      items: [
        'rules/overview',
        {
          type: 'category',
          label: 'Rule Categories',
          collapsed: false,
          items: [
            'rules/partial-functions',
            'rules/security',
            'rules/performance',
            'rules/space-leaks',
            'rules/complexity',
            'rules/architecture',
            'rules/imports',
            'rules/naming',
            'rules/pragmas',
            'rules/modernize',
            'rules/redundant',
          ],
        },
        'rules/custom-rules',
      ],
    },
    {
      type: 'category',
      label: 'CLI Reference',
      link: {
        type: 'doc',
        id: 'cli-reference/index',
      },
      items: [
        'cli-reference/check',
        'cli-reference/fix',
        'cli-reference/unused',
        'cli-reference/init',
        'cli-reference/index-cmd',
        'cli-reference/watch',
        'cli-reference/daemon',
        'cli-reference/lsp',
        'cli-reference/output-formats',
      ],
    },
    {
      type: 'category',
      label: 'Configuration',
      items: [
        'configuration/file-format',
        'configuration/rules-section',
        'configuration/naming-section',
        'configuration/complexity-section',
        'configuration/output-section',
        'configuration/fix-section',
        'configuration/scopes-section',
        'configuration/extensions-section',
      ],
    },
    {
      type: 'category',
      label: 'Architecture',
      items: [
        'architecture/overview',
        'architecture/rule-engine',
        'architecture/hie-integration',
        'architecture/fix-system',
        'architecture/analysis-passes',
        'architecture/plugin-mode',
      ],
    },
    {
      type: 'category',
      label: 'Testing',
      items: [
        'testing/running-tests',
        'testing/writing-tests',
        'testing/test-fixtures',
        'testing/golden-tests',
      ],
    },
    {
      type: 'category',
      label: 'Contributing',
      link: {
        type: 'doc',
        id: 'contributing/index',
      },
      items: [
        'contributing/development-setup',
        'contributing/code-style',
        'contributing/adding-rules',
        'contributing/testing-guide',
        'contributing/pull-requests',
      ],
    },
    {
      type: 'category',
      label: 'FAQ & Troubleshooting',
      link: {
        type: 'doc',
        id: 'faq/index',
      },
      items: [
        'faq/general',
        'faq/troubleshooting',
        'faq/error-messages',
      ],
    },
  ],
};

export default sidebars;
