import type { MDXComponents } from 'mdx/types';
import Link from 'next/link';

export function useMDXComponents(components: MDXComponents): MDXComponents {
  return {
    TOC: (props) => {
      <div>
        <Link href="/">Home</Link>
        <span className="heading">Table of Contents</span>
        {props.children}
      </div>
    },
    ...components,
  };
}