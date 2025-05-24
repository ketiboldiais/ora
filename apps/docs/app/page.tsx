import styles from "./page.module.css";
import { links, blogs } from "./links";
import Link from "next/link";
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
            This is the documentation site for Ora, a Computer Algebra System
            built with TypeScript.
          </p>
          <ol>
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
        </article>
      </main>
    </div>
  );
}
