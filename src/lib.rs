use std::fmt::{Display, Formatter};
use itertools::Itertools;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct TypeSpec {
    name: String,
    type_parameters: Vec<TypeSpec>,
    borrow: Reference,
    lifetimes: Vec<String>,
    mutable: Mutable,
}

impl TypeSpec {
    pub fn builder(name: &str) -> TypeSpecBuilder {
        TypeSpecBuilder::new(name.to_string())
    }

    pub fn to_field_string(&self) -> String {
        let generics = if !self.type_parameters.is_empty() {
            format!("{}", self.type_parameters.iter().map(|it| {
                let mut new = it.clone();
                new.mutable = Mutable::Immutable;
                new.to_field_string()
            }).join(", "))
        } else {
            "".to_string()
        };

        let lifetimes = if !self.lifetimes.is_empty() {
            format!("{}", self.lifetimes.iter().map(|it| it.prefix("'")).join(", "))
        } else {
            "".to_string()
        };

        let gen = if !generics.is_empty() && !lifetimes.is_empty() {
            format!("<{}, {}>", lifetimes, generics)
        } else if !generics.is_empty() {
            format!("<{}>", generics)
        } else if !lifetimes.is_empty() {
            format!("<{}>", lifetimes)
        } else {
            "".to_string()
        };

        format!("{}{}{}", self.borrow.pad_right(), self.name, gen)
    }

    pub fn to_parameter_string(&self) -> String {
        let generics = if !self.type_parameters.is_empty() {
            format!("{}", self.type_parameters.iter().map(|it| {
                let mut new = it.clone();
                new.mutable = Mutable::Immutable;
                new.to_field_string()
            }).join(", "))
        } else {
            "".to_string()
        };

        let lifetimes = if !self.lifetimes.is_empty() {
            format!("{}", self.lifetimes.iter().map(|it| it.prefix("'")).join(", "))
        } else {
            "".to_string()
        };

        let gen = if !generics.is_empty() && !lifetimes.is_empty() {
            format!("<{}, {}>", lifetimes, generics)
        } else if !generics.is_empty() {
            format!("<{}>", generics)
        } else if !lifetimes.is_empty() {
            format!("<{}>", lifetimes)
        } else {
            "".to_string()
        };

        format!("{}{}{}{}", self.borrow.pad_right(), self.mutable.pad_right(), self.name, gen)
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct TypeSpecBuilder {
    name: String,
    generics: Vec<TypeSpec>,
    borrow: Reference,
    lifetimes: Vec<String>,
    mutable: Mutable,
}

impl TypeSpecBuilder {
    pub fn new(name: String) -> Self {
        TypeSpecBuilder {
            name,
            generics: vec![],
            borrow: Reference::Owned,
            lifetimes: vec![],
            mutable: Mutable::Immutable,
        }
    }

    pub fn name(mut self, name: &str) -> Self {
        self.name = name.to_string();
        self
    }

    pub fn type_param(mut self, generic: TypeSpec) -> Self {
        self.generics.push(generic);
        self
    }

    pub fn type_params(mut self, generics: Vec<TypeSpec>) -> Self {
        self.generics.extend(generics);
        self
    }

    pub fn reference(mut self, borrow: Reference) -> Self {
        self.borrow = borrow;
        self
    }

    pub fn lifetime(mut self, lifetime: &str) -> Self {
        self.lifetimes.push(lifetime.to_string());
        self
    }

    pub fn lifetimes(mut self, lifetimes: Vec<String>) -> Self {
        self.lifetimes.extend(lifetimes);
        self
    }

    pub fn mutable(mut self, mutable: Mutable) -> Self {
        self.mutable = mutable;
        self
    }

    pub fn build(self) -> TypeSpec {
        TypeSpec {
            name: self.name,
            type_parameters: self.generics,
            borrow: self.borrow,
            lifetimes: self.lifetimes,
            mutable: self.mutable,
        }
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct FieldSpec {
    name: Option<String>,
    ty: Vec<TypeSpec>,
    visibility: Visibility,
    attributes: Vec<String>,
}

impl FieldSpec {
    pub fn builder() -> FieldSpecBuilder {
        FieldSpecBuilder::new()
    }
}

impl Display for FieldSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self.name.as_ref() {
            Some(name) => format!("{}: ", name),
            None => "".to_string(),
        };

        let ty = if !self.ty.is_empty() {
            if self.ty.len() == 1 {
                format!("{}", self.ty[0].to_field_string())
            } else {
                format!("({})", self.ty.iter().map(|it| it.to_field_string()).join(", "))
            }
        } else {
            "".to_string()
        };

        let attributes = if !self.attributes.is_empty() {
            self.attributes.iter().map(|it| format!("#[{}]", it)).join("\n")
        } else {
            "".to_string()
        };

        write!(f, "{}{}{}{}", attributes.suffix("\n"), self.visibility.pad_right(), name, ty)
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct FieldSpecBuilder {
    name: Option<String>,
    ty: Vec<TypeSpec>,
    visibility: Visibility,
    attributes: Vec<String>,
}

impl FieldSpecBuilder {
    pub fn new() -> Self {
        FieldSpecBuilder {
            name: None,
            ty: vec![],
            visibility: Visibility::Private,
            attributes: vec![],
        }
    }

    pub fn name(mut self, name: &str) -> Self {
        self.name = Some(name.to_string());
        self
    }

    pub fn ty(mut self, ty: TypeSpec) -> Self {
        self.ty.push(ty);
        self
    }

    pub fn visibility(mut self, visibility: Visibility) -> Self {
        self.visibility = visibility;
        self
    }

    pub fn attribute(mut self, attribute: &str) -> Self {
        self.attributes.push(attribute.to_string());
        self
    }

    pub fn build(self) -> FieldSpec {
        FieldSpec {
            name: self.name,
            ty: self.ty,
            visibility: self.visibility,
            attributes: self.attributes,
        }
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum StructSpec {
    Tuple {
        name: String,
        derives: Vec<String>,
        fields: Vec<FieldSpec>,
    },
    Named {
        name: String,
        derives: Vec<String>,
        fields: Vec<FieldSpec>,
    },
}

impl StructSpec {
    pub fn new_tuple(name: &str) -> TupleStructBuilder {
        TupleStructBuilder::new(name)
    }

    pub fn new_named(name: &str) -> NamedStructBuilder {
        NamedStructBuilder::new(name)
    }
}

impl Display for StructSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StructSpec::Tuple { name, derives, fields } => {
                let derives = if !derives.is_empty() {
                    format!("#[derive({})]\n", derives.join(", "))
                } else {
                    "".to_string()
                };

                let fields = fields.iter().map(|it| it.to_string()).join(",");

                write!(f, "{}struct {}{};\n", derives, name, fields)
            }
            StructSpec::Named { name, derives, fields } => {
                let derives = if !derives.is_empty() {
                    format!("#[derive({})]\n", derives.join(", "))
                } else {
                    "".to_string()
                };

                let fields = fields.iter().map(|it| it.prefix("    ")).join(",\n");

                write!(f, "{}struct {} {{\n{}\n}}\n", derives, name, fields)
            }
        }
    }
}

pub struct TupleStructBuilder {
    name: String,
    derives: Vec<String>,
    fields: Vec<FieldSpec>,
}

impl TupleStructBuilder {
    pub fn new(name: &str) -> Self {
        TupleStructBuilder {
            name: name.to_string(),
            derives: vec![],
            fields: vec![],
        }
    }

    pub fn derive(mut self, derive: &str) -> Self {
        self.derives.push(derive.to_string());
        self
    }

    pub fn field(mut self, field: FieldSpec) -> Self {
        self.fields.push(field);
        self
    }

    pub fn build(self) -> StructSpec {
        StructSpec::Tuple {
            name: self.name,
            derives: self.derives,
            fields: self.fields,
        }
    }
}

pub struct NamedStructBuilder {
    name: String,
    derives: Vec<String>,
    fields: Vec<FieldSpec>,
}

impl NamedStructBuilder {
    pub fn new(name: &str) -> Self {
        NamedStructBuilder {
            name: name.to_string(),
            derives: vec![],
            fields: vec![],
        }
    }

    pub fn derive(mut self, derive: &str) -> Self {
        self.derives.push(derive.to_string());
        self
    }

    pub fn field(mut self, field: FieldSpec) -> Self {
        self.fields.push(field);
        self
    }

    pub fn build(self) -> StructSpec {
        StructSpec::Named {
            name: self.name,
            derives: self.derives,
            fields: self.fields,
        }
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Crate,
    Super,
}

impl Display for Visibility {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let vis = match self {
            Visibility::Public => "pub",
            Visibility::Private => "",
            Visibility::Crate => "pub(crate)",
            Visibility::Super => "pub(super)",
        };
        write!(f, "{}", vis)
    }
}


#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Reference {
    Owned,
    Borrowed(String),
}

impl Display for Reference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Reference::Owned => write!(f, ""),
            Reference::Borrowed(lifetime) => {
                if lifetime.is_empty() {
                    write!(f, "&")
                } else {
                    write!(f, "&'{}", lifetime)
                }
            }
        }
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Mutable {
    Immutable,
    Mutable,
}

impl Display for Mutable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let vis = match self {
            Mutable::Immutable => "",
            Mutable::Mutable => "mut",
        };
        write!(f, "{}", vis)
    }
}

trait StringExt {
    fn pad_left(&self) -> String;
    fn pad_right(&self) -> String;
    fn prefix(&self, prefix: &str) -> String;
    fn suffix(&self, suffix: &str) -> String;
}

impl<T> StringExt for T where T: Display {
    fn pad_left(&self) -> String {
        self.prefix(" ")
    }

    fn pad_right(&self) -> String {
        self.suffix(" ")
    }

    fn prefix(&self, prefix: &str) -> String {
        let str = self.to_string();
        if str.is_empty() {
            str
        } else {
            prefix.to_string() + &str
        }
    }

    fn suffix(&self, suffix: &str) -> String {
        let str = self.to_string();
        if str.is_empty() {
            str
        } else {
            str + suffix
        }
    }
}

#[cfg(test)]
mod test {
    use indoc::indoc;
    use super::*;

    #[test]
    fn test_type_spec_display() {
        let expected_field = "&'a Example<'a, 'b, String, i32, &'a Example2<'a, 'b, String, i32>>".to_string();
        let expected_parameter = "&'a mut Example<'a, 'b, String, i32, &'a Example2<'a, 'b, String, i32>>".to_string();

        let complex_type = TypeSpec::builder("Example2")
            .type_param(TypeSpec::builder("String").build())
            .type_param(TypeSpec::builder("i32").build())
            .reference(Reference::Borrowed("a".to_string()))
            .lifetime("a")
            .lifetime("b")
            .mutable(Mutable::Mutable)
            .build();

        let type_spec = TypeSpec::builder("Example")
            .type_param(TypeSpec::builder("String").build())
            .type_param(TypeSpec::builder("i32").build())
            .type_param(complex_type)
            .reference(Reference::Borrowed("a".to_string()))
            .lifetime("a")
            .lifetime("b")
            .mutable(Mutable::Mutable)
            .build();

        assert_eq!(type_spec.to_field_string(), expected_field);
        assert_eq!(type_spec.to_parameter_string(), expected_parameter);
    }

    #[test]
    fn test_field_spec() {
        let expected = "pub field_name: &'a str".to_string();

        let field_spec = FieldSpec::builder()
            .name("field_name")
            .ty(TypeSpec::builder("&'a str").build())
            .visibility(Visibility::Public)
            .build();

        assert_eq!(expected, field_spec.to_string());
    }

    #[test]
    fn test_tuple_field_spec() {
        let expected = "#[serde(default)]\npub field_name: (String, String)".to_string();

        let field_spec = FieldSpec::builder()
            .name("field_name")
            .attribute("serde(default)")
            .ty(TypeSpec::builder("String").build())
            .ty(TypeSpec::builder("String").build())
            .visibility(Visibility::Public)
            .build();

        assert_eq!(expected, field_spec.to_string());
    }

    #[test]
    fn test_struct_spec() {
        let expected = indoc! {"
            #[derive(Debug, Clone)]
            struct Example {
                pub field_name: &'a str
            }
        "};

        let field_spec = FieldSpec::builder()
            .name("field_name")
            .ty(TypeSpec::builder("&'a str").build())
            .visibility(Visibility::Public)
            .build();

        let struct_spec = StructSpec::new_named("Example")
            .derive("Debug")
            .derive("Clone")
            .field(field_spec)
            .build();

        assert_eq!(expected, struct_spec.to_string());
    }

    #[test]
    fn test_tuple_struct_spec() {
        let expected = indoc! {"
            #[derive(Debug, Clone)]
            struct Example(String, String);
        "};

        let field_spec = FieldSpec::builder()
            .ty(TypeSpec::builder("String").build())
            .ty(TypeSpec::builder("String").build())
            .visibility(Visibility::Private)
            .build();

        let struct_spec = StructSpec::new_tuple("Example")
            .derive("Debug")
            .derive("Clone")
            .field(field_spec)
            .build();

        assert_eq!(expected, struct_spec.to_string());
    }
}

