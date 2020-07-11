const getDate = () => {
  const today = new Date();
  const dd = String(today.getDate()).padStart(2, "0");
  const mm = String(today.getMonth() + 1).padStart(2, "0");
  const yyyy = today.getFullYear();
  const hms = today.toTimeString().split(" ")[0];

  return mm + "/" + dd + "/" + yyyy + " " + hms;
};


const genRandomNumBetween = (min: number, max: number) => {
  return Math.random() * (max - min) + min;
};


export { getDate, genRandomNumBetween };
