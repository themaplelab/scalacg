package ca.uwaterloo.scalacg.annotation;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface MethodUID {
	public int value() default 0;
}
