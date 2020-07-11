import { Entity, Column, PrimaryGeneratedColumn } from "typeorm";

@Entity()
export default class Image {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column()
  fileName!: string;

  @Column()
  url!: string;

  @Column()
  stamp!: string;

  @Column()
  hash!: string;

  @Column({ type: "bigint" })
  spawn!: number;

  @Column()
  ip!: string;
}
