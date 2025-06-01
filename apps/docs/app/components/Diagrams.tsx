import tokens1 from "../diagrams/tokens1.svg";
import runtime_classes from "../diagrams/runtime_classes.svg";
import { Img } from "./Img";

export const TOKENS1 = () => (
  <Img width={200} url={tokens1} alt={"Token sequence"} />
);

export const RUNTIME_CLASSES = () => (
  <Img width={350} url={runtime_classes} alt={"Ora runtime classes hierarchy"} />
);