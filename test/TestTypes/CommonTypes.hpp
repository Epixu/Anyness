#pragma once
#include <Langulus/Typenav.hpp>
#include <Langulus/Tag.hpp>

using namespace Langulus;
using RTTI::DMeta;
using RTTI::TMeta;
using RTTI::CMeta;
using RTTI::VMeta;


class IncompleteType;


template<class T>
struct SheddableType { using CTTI_Sheddable = T; };

enum class Pi {
   Number = 314,
   ConflictingNumber = 666
};

struct NotReflectable       { using CTTI_ReflectAs = void; };
struct NotReflectableIntern { using CTTI_ReflectAs = No;   };
struct NotReflectableExtern {};
struct ReflectableIntern    { using CTTI_ReflectAs = char; };
struct ReflectableExtern    {};
struct ReflectableAsSelf    { using CTTI_ReflectAs = ReflectableAsSelf; };

namespace Langulus::CTTI
{
   template<>
   struct ReflectAs<NotReflectableExtern> {
      using Type = void;
   };

   template<>
   struct ReflectAs<ReflectableExtern> {
      using Type = char;
   };
}

namespace Langulus::Tags
{
   struct Name {
      using CTTI_Versioned = Version<7, 10>;
      using CTTI_DefineTag = Yes<"Name">;
      using CTTI_Info      = Yes<"Used for tagging names">;
   };
   struct ConflictingName1 {
      using CTTI_DefineTag = Yes<"Name">;
   };
   struct ConflictingName2 {
      using CTTI_DefineTag = Yes<"int">;
   };
   struct ConflictingName3 {
      using CTTI_DefineTag = Yes<"create">;
   };
   struct ConflictingName4 {
      using CTTI_DefineTag = Yes<"Pi::Number">;
   };
}

namespace Langulus::Flow
{
   struct Verb {};
}

namespace Langulus::Verbs
{
   /// Defines a verb                                                         
   struct Create {
      using CTTI_Versioned = Version<6, 10>;
      using CTTI_DefineVerb = DefineVerb<"create", "destroy", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" + ", " - ">;
      using CTTI_Info = Yes<
         "Used for allocating new elements. "
         "If the type you're creating has	a producer, "
         "you need to execute the verb in a matching producer, "
         "or that producer will be created automatically for you, if possible"
      >;

      /// Allows the verb to be executed without context                      
      static bool ExecuteContextless(Flow::Verb&) {
         Logger::Special("Verbs::Create executed without context");
         return true;
      }

      /// Helps you specialize verbs for types/concepts                       
      /// You can also do that, by adding a `void Create(Flow::Verb&)` in T   
      template<class T> struct In {
         static bool Execute(T& context, Flow::Verb& v)
         requires (requires { context.Create(v); }) {
            Logger::Special("Verbs::Create executed using method in: ", NameOf<T>());
            context.Create(v);
            return true;
         }
      };

      /// Checks whether T is capable of doing this verb                      
      template<class T>
      static constexpr bool IsAble = requires (T& t, Flow::Verb& v) {
         Create::In<T>::Execute(t, v);
      };
   };

   /// Specializing for any other type                                        
   template<>
   struct Create::In<DMeta> {
      static bool Execute(DMeta& context, Flow::Verb&) {
         Logger::Special("Verbs::Create executed in: ", context.GetName());
         return true;
      }
   };

   template<>
   struct Create::In<const DMeta> {
      static bool Execute(const DMeta& context, Flow::Verb&) {
         Logger::Special("Verbs::Create executed in: ", context.GetName());
         return true;
      }
   };

   struct ConflictingByPosToken1 {
      using CTTI_DefineVerb = DefineVerb<"create", "destroy_alt", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" ++ ", " -- ">;
   };
   struct ConflictingByPosToken2 {
      using CTTI_DefineVerb = DefineVerb<"int", "destroy_alt", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" ++ ", " -- ">;
   };
   struct ConflictingByPosToken3 {
      using CTTI_DefineVerb = DefineVerb<"name", "destroy_alt", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" ++ ", " -- ">;
   };

   struct ConflictingByNegToken1 {
      using CTTI_DefineVerb = DefineVerb<"create_alt", "destroy", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" ++ ", " -- ">;
   };
   struct ConflictingByNegToken2 {
      using CTTI_DefineVerb = DefineVerb<"create_alt2", "int", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" +++ ", " --- ">;
   };
   struct ConflictingByNegToken3 {
      using CTTI_DefineVerb = DefineVerb<"create_alt", "name", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" ++ ", " -- ">;
   };

   struct ConflictingByPosOp1 {
      using CTTI_DefineVerb = DefineVerb<"create_alt", "destroy_alt", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" + ", " -- ">;
   };
   struct ConflictingByPosOp2 {
      using CTTI_DefineVerb = DefineVerb<"create_alt3", "destroy_alt3", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" int ", " ---- ">;
   };
   struct ConflictingByPosOp3 {
      using CTTI_DefineVerb = DefineVerb<"create_alt", "destroy_alt", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" name ", " -- ">;
   };

   struct ConflictingByNegOp1 {
      using CTTI_DefineVerb = DefineVerb<"create_alt", "destroy_alt", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" ++ ", " - ">;
   };
   struct ConflictingByNegOp2 {
      using CTTI_DefineVerb = DefineVerb<"create_alt4", "destroy_alt4", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" ++++ ", " int ">;
   };
   struct ConflictingByNegOp3 {
      using CTTI_DefineVerb = DefineVerb<"create_alt", "destroy_alt", 5.f>;
      using CTTI_DefineVerbOperator = DefineVerb<" ++ ", " name ">;
   };
}



struct ConvertibleToInt {
   using CTTI_MapsTo = int;

   ConvertibleToInt(int inner = 666)
      : member{inner} {}

   explicit operator int() const noexcept {
      return member;
   }

private:
   int member;
};

struct ImplicitlyReflectedData {
   enum Named { One, Two, Three };

   using CTTI_POD    = Yes<>;
   using CTTI_Files  = Yes<"ASE">;
   using CTTI_Typed  = Named;
   using CTTI_Values = Values<One, Two, Three>;

   Named v = One;

   inline bool operator == (const ImplicitlyReflectedData&) const noexcept = default;
};

class alignas(128) ImplicitlyReflectedDataWithTraits : public ImplicitlyReflectedData {
public:
   int member {664};
   Tag<bool, Tags::Name> anotherMember {};
   int anotherMemberArray [12] {};
   int* sparseMember {};

   inline operator int() const noexcept {
      return member;
   }

   void Create(Flow::Verb&) const {
      //++member;
   }

   void Create(Flow::Verb&) {
      ++member;
   }

   ImplicitlyReflectedDataWithTraits() = default;
   explicit ImplicitlyReflectedDataWithTraits(Pi)
      : member {314} {}

   using CTTI_Named     = Yes<"MyType">;
   using CTTI_Info      = Yes<"Info about MyType">;
   using CTTI_Files     = Yes<"txt, pdf">;
   using CTTI_Versioned = Version<2, 1>;
   using CTTI_Deep      = Yes<>;
   using CTTI_Nullable  = Yes<>;
   using CTTI_Pooled    = PooledBySize;
   using CTTI_Concrete  = ImplicitlyReflectedData;
   using CTTI_Abstract  = Yes<>;
   using CTTI_Bases     = ImplicitlyReflectedData;
   using CTTI_Verbs     = Verbs::Create;
   using CTTI_MapsTo    = int;
   using CTTI_Values    = No;
   using CTTI_MinAlloc  = Yes<1024>;

   using Self = ImplicitlyReflectedDataWithTraits;
   using CTTI_Members = Members<
      &Self::member,
      &Self::anotherMember,
      &Self::anotherMemberArray,
      &Self::sparseMember
   >;
};

struct ConvertibleData : ImplicitlyReflectedData {
   int member {664};

   inline operator int() const noexcept {
      return member;
   }

   ConvertibleData() = default;
   explicit ConvertibleData(Pi)
      : member {314} {}

   using CTTI_Bases     = ImplicitlyReflectedData;
   using CTTI_MapsTo    = int;
   using CTTI_Values    = No;
};

struct ConflictingName { using CTTI_Named = Yes<"MyType">;   };
struct InvalidName1    { using CTTI_Named = Yes<"1MyType">;  };
struct InvalidName2    { using CTTI_Named = Yes<"MyType{}">; };
struct InvalidName3    { using CTTI_Named = Yes<"My.Type">;  };
struct InvalidName4    { using CTTI_Named = Yes<"MyType[]">; };
struct InvalidName5    { using CTTI_Named = Yes<"MyType,">;  };
struct InvalidName6    { using CTTI_Named = Yes<"My Type">;  };

struct ReservedName1   { using CTTI_Named = Yes<"const">;    };
struct ReservedName2   { using CTTI_Named = Yes<"null">;     };
struct ReservedName3   { using CTTI_Named = Yes<"noverb">;   };
struct ReservedName4   { using CTTI_Named = Yes<"nodata">;   };
struct ReservedName5   { using CTTI_Named = Yes<"novalue">;  };
struct ReservedName6   { using CTTI_Named = Yes<"notag">;    };

struct CheckingWhatGetsInherited : ImplicitlyReflectedDataWithTraits {
   using CTTI_Named = Yes<"CheckingWhatGetsInherited">;
   using ImplicitlyReflectedDataWithTraits::ImplicitlyReflectedDataWithTraits;
};

/*void FunctionForTesting(void*) {
   Logger::Verbose("Executed FunctionForTesting");
}*/


/// Built-in abstract type via a pure virtual function                     
struct PureAbstract {
   PureAbstract() = delete;
   virtual ~PureAbstract() {}
   PureAbstract(void*) {}
   [[maybe_unused]] virtual auto PureVirtualMethod() -> size_t = 0;
};

/// Proper type, reflected as abstract                                     
struct ForcedAbstractExternally {};
struct ForcedAbstractInternally {
   using CTTI_Abstract = Yes<>;
};

/// Types that can inherit abstractness                                    
struct InheritedAbstract1 : ForcedAbstractInternally { };
struct InheritedAbstract2 : PureAbstract { };

/// Types that can inherit abstractness privately                          
struct ImpureVirtual {
   virtual ~ImpureVirtual() {}
};
struct InheritedAbstract1ButPrivate : private ForcedAbstractInternally {};
struct InheritedAbstract2ButPrivate : private PureAbstract {};
struct InheritedAbstractExternally  : ForcedAbstractExternally {};




/// Type that has a virtual base                                           
struct VirtuallyDerived : virtual ImpureVirtual {
   using CTTI_Bases = Types<ImpureVirtual, int>;
};

/// Type that has a private non-virtual base                               
struct PrivatelyDerived : private ImpureVirtual {
   using CTTI_Bases = Types<ImpureVirtual, int, float>;
};




///                                                                        
/// Convertible from int                                                   
struct BuiltinConvertibleFromIntViaConstructor {
   int inner = 0;
   BuiltinConvertibleFromIntViaConstructor(int x) : inner {x} {}
};

struct BuiltinConvertibleFromIntViaExplicitConstructor {
   int inner = 0;
   explicit BuiltinConvertibleFromIntViaExplicitConstructor(int x) : inner {x} {}
};

struct ConvertibleFromIntExternallyMissingConverter {};
class  ConvertibleFromIntExternally {
   int inner = 0;
public:
   static ConvertibleFromIntExternally Init(int i) { 
      ConvertibleFromIntExternally temp;
      temp.inner = i;
      return temp;
   }
};

struct ConvertibleFromIntInternallyMissingConverter {
   using CTTI_MapsFrom = int;
};

class ConvertibleFromIntInternally {
   int inner = 0;
public:
   using CTTI_MapsFrom = int;
   static ConvertibleFromIntInternally Init(int i) { 
      ConvertibleFromIntInternally temp;
      temp.inner = i;
      return temp;
   }
};

/// Types that inherit convertible properties                              
struct InheritedConvertibleFromInt1
   : ConvertibleFromIntInternally {};
struct InheritedConvertibleFromInt1Disabled
   : ConvertibleFromIntInternally { using CTTI_MapsFrom = void; };
struct InheritedConvertibleFromInt2
   : ConvertibleFromIntExternally {};
struct InheritedConvertibleFromInt3
   : BuiltinConvertibleFromIntViaConstructor {};
struct InheritedConvertibleFromInt4
   : BuiltinConvertibleFromIntViaExplicitConstructor {};

/// Types that inherit convertible properties privately                    
struct InheritedConvertibleFromInt1ButPrivate : private ConvertibleFromIntInternally {};
struct InheritedConvertibleFromInt2ButPrivate : private BuiltinConvertibleFromIntViaConstructor {};
struct InheritedConvertibleFromInt3ButPrivate : private BuiltinConvertibleFromIntViaExplicitConstructor {};
struct InheritedConvertibleFromIntExternally : ConvertibleFromIntExternally {};


///                                                                        
/// Convertible to int                                                     
class BuiltinConvertibleToIntViaOperator {
   int inner = 0;
public:
   operator int() const noexcept {
      return inner;
   }
};

class BuiltinConvertibleToIntViaOperatorMutable {
   int inner = 0;
public:
   operator int() noexcept {
      return inner;
   }
};

class BuiltinConvertibleToIntViaExplicitOperator {
   int inner = 0;
public:
   explicit operator int() const noexcept {
      return inner;
   }
};

class BuiltinConvertibleToIntViaExplicitOperatorMutable {
   int inner = 0;
public:
   explicit operator int() noexcept {
      return inner;
   }
};

struct BuiltinConvertibleToIntBecauseAggregate {
   int inner = 0;
};

struct ConvertibleToIntExternallyMissingConverter {};
struct ConvertibleToIntExternally {
   ::std::string inner;
};

struct ConvertibleToIntInternallyMissingConverter {
   using CTTI_MapsTo = int;
};

struct ConvertibleToIntInternally {
   ::std::string inner;
   using CTTI_MapsTo = int;
};

/// Types that inherit convertible properties                              
struct InheritedConvertibleToInt1
   : ConvertibleToIntInternally {};
struct InheritedConvertibleToInt1Disabled
   : ConvertibleToIntInternally { using CTTI_MapsTo = void; };
struct InheritedConvertibleToInt2
   : ConvertibleToIntExternally {};
struct InheritedConvertibleToInt3
   : BuiltinConvertibleToIntViaOperator {};
struct InheritedConvertibleToInt4
   : BuiltinConvertibleToIntViaOperatorMutable {};
struct InheritedConvertibleToInt5
   : BuiltinConvertibleToIntViaExplicitOperator {};
struct InheritedConvertibleToInt6
   : BuiltinConvertibleToIntViaExplicitOperatorMutable {};

/// Types that inherit convertible properties privately                    
struct InheritedConvertibleToInt1ButPrivate : private ConvertibleToIntInternally {};
struct InheritedConvertibleToInt2ButPrivate : private BuiltinConvertibleToIntViaOperator {};
struct InheritedConvertibleToInt3ButPrivate : private BuiltinConvertibleToIntViaExplicitOperator {};
struct InheritedConvertibleToIntExternally : ConvertibleToIntExternally {};

/// For testing container related concepts                                    
struct CustomContainer {
   using CTTI_Container = Yes<>;
};


namespace Langulus::CTTI
{
   /// These customizations need to appear as early as possible, in order     
   /// to be consistently reflected in all tests                              
   template<>
   struct MapsTo<int> {
      using From = ::std::string;
   };

   template<>
   struct Converter<::std::string, int> {
      static constexpr auto Convert(::std::string const& from) -> int {
         return from == "the devil" ? 666 : -1;
      }
   };

   template<>
   struct Named<::std::string> {
      static constexpr Literal Name = "string";
   };

   template<>
   struct MapsFrom<Pi> {
      using To = Types<ImplicitlyReflectedDataWithTraits, ConvertibleData>;
   };

   template<>
   struct MapsTo<ImplicitlyReflectedDataWithTraits> {
      using From = Pi;
   };

   template<>
   struct NamedValue<Pi::ConflictingNumber> {
      static constexpr Literal Name = "Pi::Number";
   };

   template<>
   struct Abstract<ForcedAbstractExternally> {};
   
   template<>
   struct Verbs<DMeta> {
      using Type = Langulus::Verbs::Create;
   };


   template<>
   struct MapsTo<ConvertibleFromIntExternallyMissingConverter> {
      using From = int;
   };
   template<>
   struct MapsTo<ConvertibleFromIntExternally> {
      using From = int;
   };
   template<>
   struct Converter<int, ConvertibleFromIntExternally> {
      static constexpr auto Convert(int const& from) -> ConvertibleFromIntExternally {
         return ConvertibleFromIntExternally::Init(from);
      }
   };
   template<>
   struct Converter<int, ConvertibleFromIntInternally> {
      static constexpr auto Convert(int const& from) -> ConvertibleFromIntInternally {
         return ConvertibleFromIntInternally::Init(from);
      }
   };


   template<>
   struct MapsFrom<ConvertibleToIntExternallyMissingConverter> {
      using To = int;
   };
   template<>
   struct MapsFrom<ConvertibleToIntExternally> {
      using To = int;
   };
   template<>
   struct Converter<ConvertibleToIntExternally, int> {
      static constexpr auto Convert(ConvertibleToIntExternally const& from) -> int {
         return from.inner.size();
      }
   };
   template<>
   struct Converter<ConvertibleToIntInternally, int> {
      static constexpr auto Convert(ConvertibleToIntInternally const& from) -> int {
         return from.inner.size();
      }
   };
}
