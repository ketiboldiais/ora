import type { MDXComponents } from "mdx/types";
import Link from "next/link";
import { ReactNode, CSSProperties } from "react";

export function useMDXComponents(components: MDXComponents): MDXComponents {
  return {
    a: (props) => (
      <Link href={props.href} target="rel">
        {props.children}
      </Link>
    ),
    Cols: (props: { children: ReactNode; of?: number }) => {
      const columnCount = props.of ?? 2;
      const style: CSSProperties = {
        display: "grid",
        gridTemplateColumns: `repeat(${columnCount}, 1fr)`,
      };
      return <div style={style}>{props.children}</div>;
    },
    Example: (props) => (
      <div className="example">
        <div className="example-title">example.</div>
        <div className="example-body">
          {props.children}
        </div>
      </div>
    ),
    Procedure: (props) => <div className="procedure">{props.children}</div>,
    TOC: (props) => (
      <div className={"toc"}>
        <Link href="/">Home</Link>
        <span className="heading">Table of Contents</span>
        {props.children}
      </div>
    ),
    ...components,
  };
}
