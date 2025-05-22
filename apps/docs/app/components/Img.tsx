import Image from "next/image";

export const Img = ({
  url,
  alt,
  width,
  height,
}: {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  url: any;
  alt: string;
  width?: number;
  height?: number;
}) => (
  <figure
    style={{ width: "fit-content", marginTop: "20px", marginBottom: "20px" }}
  >
    <Image width={width} height={height} src={url} alt={alt} />
  </figure>
);
