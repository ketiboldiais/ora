import styles from "./page.module.css";
import { links, blogs } from "./links";
import Link from "next/link";
// import {int} from "@ora/engine/main";

export default function Home() {
  return (
    <div className={styles.page}>
      <main className={styles.main}>
        <h1>Ora</h1>
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
          <li>Blog</li>
          <ul>
            {blogs.map((blog) => (
              <li key={blog.url}>
                <Link href={"blog/" + blog.url}>{blog.title}</Link>
              </li>
            ))}
          </ul>
        </ol>
      </main>
    </div>
  );
}
