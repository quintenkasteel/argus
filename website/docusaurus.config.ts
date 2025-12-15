import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'Argus',
  tagline: 'The All-Seeing Haskell Static Analyzer',
  favicon: 'img/favicon.ico',

  // GitHub Pages deployment configuration
  // Deployed to https://quintenkasteel.github.io/argus/
  url: 'https://quintenkasteel.github.io',
  baseUrl: '/argus/',

  // GitHub user/org name and repo name
  organizationName: 'quintenkasteel',
  projectName: 'argus',

  // Deployment settings
  trailingSlash: false,
  deploymentBranch: 'gh-pages',

  onBrokenLinks: 'warn',
  onBrokenMarkdownLinks: 'warn',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          editUrl: 'https://github.com/quintenkasteel/argus/tree/main/website/',
          showLastUpdateTime: true,
          showLastUpdateAuthor: true,
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    image: 'img/argus-social-card.png',
    navbar: {
      title: 'Argus',
      logo: {
        alt: 'Argus Logo',
        src: 'img/logo.svg',
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'docsSidebar',
          position: 'left',
          label: 'Documentation',
        },
        {
          to: '/docs/cli-reference',
          label: 'CLI Reference',
          position: 'left',
        },
        {
          to: '/docs/rules/overview',
          label: 'Rules',
          position: 'left',
        },
        {
          href: 'https://github.com/quintenkasteel/argus',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Documentation',
          items: [
            {
              label: 'Getting Started',
              to: '/docs/getting-started/installation',
            },
            {
              label: 'Usage Guide',
              to: '/docs/usage-guide/analysis-modes',
            },
            {
              label: 'Rules Reference',
              to: '/docs/rules/overview',
            },
          ],
        },
        {
          title: 'Reference',
          items: [
            {
              label: 'CLI Reference',
              to: '/docs/cli-reference',
            },
            {
              label: 'Configuration',
              to: '/docs/configuration/file-format',
            },
            {
              label: 'Architecture',
              to: '/docs/architecture/overview',
            },
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/quintenkasteel/argus',
            },
            {
              label: 'Contributing',
              to: '/docs/contributing',
            },
            {
              label: 'Issues',
              href: 'https://github.com/quintenkasteel/argus/issues',
            },
          ],
        },
      ],
      copyright: `Copyright ${new Date().getFullYear()} Argus Project. Built with Docusaurus.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['haskell', 'toml', 'bash', 'yaml', 'json'],
    },
    algolia: undefined,
    colorMode: {
      defaultMode: 'light',
      disableSwitch: false,
      respectPrefersColorScheme: true,
    },
    tableOfContents: {
      minHeadingLevel: 2,
      maxHeadingLevel: 4,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
