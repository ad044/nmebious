import { Entity, Column, PrimaryGeneratedColumn } from "typeorm";

@Entity()
export default class Ban {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column()
  ip!: string;
}
