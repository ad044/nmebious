import { Entity, Column, PrimaryGeneratedColumn } from "typeorm";

@Entity()
export default class Text {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column()
  text!: string;

  @Column()
  stamp!: string;

  @Column()
  spawn!: string;

  @Column()
  ip!: string;
}
