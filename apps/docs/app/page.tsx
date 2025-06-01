import styles from "./page.module.css";
import { links, blogs } from "./links";
import Link from "next/link";
import { Tex } from "./components/Tex";
// import {lexicalAnalyzer} from "@ora/engine/main";

export default function Home() {
  // const r = lexicalAnalyzer('1_000 + 12 + x').stream().map(l => l.map(t => t.toString()));
  // console.log(r);
  return (
    <div className={styles.page}>
      <main className={styles.main}>
        <h1>Ora</h1>
        <article className="article-main">
          <p>
            This is the documentation site for Ora, a CAS built with TypeScript.
          </p>
          <ol className="main-toc">
            {links.map((link) => (
              <li key={link.url}>
                <Link href={"pages/" + link.url}>{link.title}</Link>
              </li>
            ))}
            <li>
              Blog
              <ol>
                {blogs.map((blog) => (
                  <li key={blog.url}>
                    <Link href={"blog/" + blog.url}>{blog.title}</Link>
                  </li>
                ))}
              </ol>
            </li>
          </ol>
          <span className="header">FAQ</span>
          <p>
            <b>What&apos;s a CAS?</b> A CAS (Computer Algebra System) is a
            computer program that performs symbolic mathematical operations.
            These are operations like:
          </p>
          <div className={"ident"}>
            <p>
              Solve for <Tex content="x" />:{" "}
              <Tex content="7x^2 - 5x + 6 = 117." />
            </p>
            <p>
              Simplify: <Tex content="\dfrac{3x + x + 1}{xy}." />
            </p>
            <p>
              Differentiate:{" "}
              <Tex content="f(x) = \dfrac{\sqrt{2x^2 - \ln x}}{5x}." />
            </p>
          </div>
          <p>
            If you&apos;ve ever had to take a college math class, you&apos;ve
            probably encountered a few of them: <i>Mathematica</i>, <i>SymPy</i>
            , <i>SageMath</i>, etc.
          </p>
          <p>
            <b>Why build one?</b> This project started off as a small graphics
            library to help publish my notes from undergrad and law school. That
            library grew bigger and messier as my courses &mdash; and their
            accompanying notes &mdash; grew more complicated. Ora&apos;s my
            attempt at cleaning it up into a single package.
          </p>
          <p>
            I also love math &mdash; developing this library&apos;s been a great
            learning experience. Without question, making a CAS is <i>hard</i>,
            and it&apos;s arguably a fool&apos;s errand. You run into efficiency
            issues, you must constantly consider trade-offs between
            extensability and performance, and you&apos;re forced to do research
            about a lot of different areas of mathematics. But, that difficulty
            is what makes working on this project enjoyable.
          </p>
          <p>
            <b>What&apos;s with the name? Ora?</b> Latin. As a noun, it means
            &ldquo;shore&rdquo; or &ldquo;coast.&rdquo; Developing Ora feels
            like standing on the edge of computational capability (e.g., per{" "}
            <i>Hilbert&apos;s Tenth Problem</i>, there&apos;s no general
            algorithm for determining whether any Diophantine equation has a
            solution). As a verb, it means to &ldquo;pray&rdquo; or
            &ldquo;plead&rdquo; &mdash; I run a lot of tests with this library,
            and the most common phrase I hear myself saying is &ldquo;Please
            work.&rdquo;
          </p>
        </article>
      </main>
      <footer>Ketib Oldiais © 2025</footer>
    </div>
  );
}
