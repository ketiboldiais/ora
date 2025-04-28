// Import the MDX plugin
import nextMDX from "@next/mdx";

/** @type {import('next').NextConfig} */
const nextConfig = {
  // Configure pageExtensions to support MDX
  pageExtensions: ["js", "jsx", "mdx", "ts", "tsx"],
};

const withMDX = nextMDX({
  options: {
    remarkPlugins: [['remark-math'], ['remark-gfm']],
    rehypePlugins: [['rehype-katex'], ['rehype-highlight']],
  },
});


export default withMDX(nextConfig);
