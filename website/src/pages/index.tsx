import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';

import styles from './index.module.css';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary hero--argus', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className="hero__title">
          {siteConfig.title}
        </Heading>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <p className={styles.heroDescription}>
          Enterprise-grade static analysis for Haskell combining syntactic pattern matching,
          semantic HIE analysis, and GHC plugin integration.
        </p>
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/overview/quickstart">
            Get Started in 5 Minutes
          </Link>
          <Link
            className="button button--outline button--lg"
            style={{marginLeft: '1rem', color: 'white', borderColor: 'white'}}
            to="/docs/overview/introduction">
            Learn More
          </Link>
        </div>
      </div>
    </header>
  );
}

type FeatureItem = {
  title: string;
  description: JSX.Element;
  icon: string;
};

const FeatureList: FeatureItem[] = [
  {
    title: 'Multi-Mode Analysis',
    icon: '🔍',
    description: (
      <>
        Choose your analysis depth: quick syntax-only checks for rapid iteration,
        full HIE-based semantic analysis for production, or GHC plugin integration
        for compile-time precision.
      </>
    ),
  },
  {
    title: 'Smart Unused Detection',
    icon: '🎯',
    description: (
      <>
        Dependency graph analysis with Template Haskell awareness finds truly unused
        code, respecting TH-generated functions and configurable entry points.
      </>
    ),
  },
  {
    title: 'Security Analysis',
    icon: '🔒',
    description: (
      <>
        Catch injection vulnerabilities, hardcoded secrets, unsafe FFI calls,
        cryptography issues, and leftover debug code before they reach production.
      </>
    ),
  },
  {
    title: 'Performance Detection',
    icon: '⚡',
    description: (
      <>
        Identify space leaks, fusion blockers, lazy/strict issues, inefficient
        data structures, and algorithmic anti-patterns that slow your code.
      </>
    ),
  },
  {
    title: 'Safe Auto-Fix',
    icon: '🔧',
    description: (
      <>
        Transactional refactoring with conflict detection, validation, and rollback.
        Preview changes, apply interactively, or run in batch with confidence.
      </>
    ),
  },
  {
    title: 'CI/CD Ready',
    icon: '🚀',
    description: (
      <>
        SARIF output for GitHub Code Scanning, JSON for custom tooling, JUnit for
        test frameworks. Integrates with GitHub Actions, GitLab CI, and more.
      </>
    ),
  },
];

function Feature({title, icon, description}: FeatureItem) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center padding-horiz--md feature-card">
        <div style={{fontSize: '3rem', marginBottom: '1rem'}}>{icon}</div>
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
    </div>
  );
}

function HomepageFeatures(): JSX.Element {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}

function QuickInstall(): JSX.Element {
  return (
    <section className={styles.quickInstall}>
      <div className="container">
        <Heading as="h2" className="text--center">
          Quick Install
        </Heading>
        <div className={styles.installCode}>
          <pre>
            <code>
{`# Clone and build
git clone https://github.com/quinten/argus.git
cd argus
stack build
stack install

# Run on your project
argus check src/`}
            </code>
          </pre>
        </div>
        <div className="text--center" style={{marginTop: '1rem'}}>
          <Link to="/docs/getting-started/installation">
            View full installation guide →
          </Link>
        </div>
      </div>
    </section>
  );
}

function RuleCategories(): JSX.Element {
  const categories = [
    {name: 'Security', color: '#dc2626', count: 8},
    {name: 'Performance', color: '#d97706', count: 12},
    {name: 'Space Leaks', color: '#7c3aed', count: 5},
    {name: 'Partial Functions', color: '#2563eb', count: 12},
    {name: 'Modernization', color: '#059669', count: 11},
    {name: 'Complexity', color: '#db2777', count: 6},
    {name: 'Architecture', color: '#0891b2', count: 4},
    {name: 'Imports', color: '#65a30d', count: 4},
  ];

  return (
    <section className={styles.ruleCategories}>
      <div className="container">
        <Heading as="h2" className="text--center">
          1,100+ Rules Across 16 Categories
        </Heading>
        <p className="text--center" style={{marginBottom: '2rem'}}>
          Comprehensive coverage from partial function detection to architectural analysis
        </p>
        <div className={styles.categoryGrid}>
          {categories.map((cat) => (
            <div
              key={cat.name}
              className={styles.categoryCard}
              style={{'--category-color': cat.color} as React.CSSProperties}
            >
              <span className={styles.categoryName}>{cat.name}</span>
              <span className={styles.categoryCount}>{cat.count} rules</span>
            </div>
          ))}
        </div>
        <div className="text--center" style={{marginTop: '2rem'}}>
          <Link
            className="button button--primary button--lg"
            to="/docs/rules/overview">
            Explore All Rules
          </Link>
        </div>
      </div>
    </section>
  );
}

export default function Home(): JSX.Element {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title={`${siteConfig.title} - ${siteConfig.tagline}`}
      description="Enterprise-grade static analysis tool for Haskell combining syntactic pattern matching, semantic HIE analysis, and GHC plugin integration.">
      <HomepageHeader />
      <main>
        <HomepageFeatures />
        <QuickInstall />
        <RuleCategories />
      </main>
    </Layout>
  );
}
