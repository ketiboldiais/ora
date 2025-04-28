import styles from "./page.module.css";
import { links } from "./links";
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
              <Link href={"pages/"+link.url}>{link.title}</Link>
            </li>
          ))}
        </ol>
      </main>
    </div>
  );
}
