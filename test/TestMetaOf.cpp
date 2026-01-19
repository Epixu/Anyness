///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/MetaOf.hpp>
#include <Langulus/Utils/Values.hpp>
#include <Langulus/Tag.hpp>
#include <Langulus/CT/Members.hpp>

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #include "TestTypes/PackedPointers.hpp"
#endif

using namespace Langulus;
using RTTI::DMeta;
using RTTI::TMeta;
using RTTI::CMeta;
using RTTI::VMeta;

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

namespace
{
   class IncompleteType;
   
   enum class Pi {
      Number = 314,
      ConflictingNumber = 666
   };
   
   struct NotReflectable    { using CTTI_ReflectAs = void; };
   struct NotReflectableAlt { using CTTI_ReflectAs = No;   };

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
}

namespace Langulus::CTTI
{
   template<>
   struct MapsTo<Pi> {
      using Type = Types<ImplicitlyReflectedDataWithTraits, ConvertibleData>;
   };
   template<>
   struct NamedValue<Pi::ConflictingNumber> {
      static constexpr Literal Name = "Pi::Number";
   };
}


///                                                                           
/// Testing empty meta data                                                   
///                                                                           
TEST_CASE("Testing empty meta data", "[rtti]") {
   const DMeta meta = {};
   REQUIRE_FALSE(meta);
   REQUIRE(meta.GetHash() == Hash {});
   REQUIRE(meta.GetCppName() == "");
   REQUIRE(meta.GetName() == RTTI::DefinitionData::InvalidName);
   REQUIRE(meta.GetInfo() == "");
   REQUIRE(meta.GetFiles() == "");
   REQUIRE(meta.GetSuffix() == "");
   REQUIRE(meta.GetVersionMajor() == 0);
   REQUIRE(meta.GetVersionMinor() == 0);

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      REQUIRE(meta.GetID() == 0);
      REQUIRE(meta.GetBoundaries().empty());
   #endif

   REQUIRE(meta.GetOrigin() == nullptr);
   REQUIRE(meta.GetDeptr() == nullptr);
   REQUIRE(meta.GetDecvqAll() == nullptr);
   REQUIRE(meta.GetDecvq() == nullptr);
   REQUIRE(meta.AddPtr() == nullptr);
   REQUIRE(meta.AddConst() == nullptr);
   
   REQUIRE(meta.GetSize() == 0);
   REQUIRE(meta.GetAlignment() == Alignment);
   REQUIRE(meta.IsConstant() == false);
   REQUIRE(meta.IsDeep() == false);
   REQUIRE(meta.IsPOD() == false);
   REQUIRE(meta.IsNullable() == false);
   REQUIRE(meta.IsAbstract() == false);
   REQUIRE(meta.HasGetHashMethod() == false);

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      REQUIRE(meta.GetMinPoolsize() == MinimalPoolSize);
      REQUIRE(meta.GetPoolTactic() == PoolTactic::Default);
   #endif
   
   REQUIRE(meta.GetDefaultConstructor() == nullptr);
   REQUIRE(meta.GetDescribeConstructor()== nullptr);
   REQUIRE(meta.GetReferConstructor()   == nullptr);
   REQUIRE(meta.GetCopyConstructor()    == nullptr);
   REQUIRE(meta.GetDisownConstructor()  == nullptr);
   REQUIRE(meta.GetCloneConstructor()   == nullptr);
   REQUIRE(meta.GetMoveConstructor()    == nullptr);
   REQUIRE(meta.GetAbandonConstructor() == nullptr);
   
   REQUIRE(meta.GetDestructor()      == nullptr);
   REQUIRE(meta.GetComparer()        == nullptr);
   REQUIRE(meta.GetComparerEqual()   == nullptr);

   REQUIRE(meta.GetReferAssigner()   == nullptr);
   REQUIRE(meta.GetCopyAssigner()    == nullptr);
   REQUIRE(meta.GetDisownAssigner()  == nullptr);
   REQUIRE(meta.GetCloneAssigner()   == nullptr);
   REQUIRE(meta.GetMoveAssigner()    == nullptr);
   REQUIRE(meta.GetAbandonAssigner() == nullptr);

   REQUIRE(meta.GetResolver()   == nullptr);
   REQUIRE(meta.GetHasher()     == nullptr);
   REQUIRE(meta.GetReferencer() == nullptr);
   REQUIRE(meta.GetDispatcher() == nullptr);
   REQUIRE(meta.GetConcrete()   == nullptr);
   REQUIRE(meta.GetProducer()   == nullptr);

   REQUIRE(meta.GetMembers().size() == 0);
   REQUIRE(meta.GetVerbs().size() == 0);
   REQUIRE(meta.GetBases().size() == 0);
   REQUIRE(meta.GetMorphismsTo().size() == 0);
   REQUIRE(meta.GetMorphismsFrom().size() == 0);
   REQUIRE(meta.GetNamedValues().size() == 0);
}

///                                                                           
/// Reflecting incomplete types                                               
///                                                                           
TEMPLATE_TEST_CASE("Testing reflection of incomplete types", "[rtti]"
   //void,           // shouldn't compile
   //nullptr_t,      // shouldn't compile
   //IncompleteType, // shouldn't compile
   //NotReflectable, // shouldn't compile
   , IncompleteType*
   , IncompleteType**
   , const IncompleteType**
   , const IncompleteType* const*&
   , const IncompleteType* const* const&
   , const IncompleteType* const* const&&
) {
   using T = TestType;
   const DMeta meta = MetaDataOf<T>();
   REQUIRE(meta);
   REQUIRE(meta != nullptr);
   REQUIRE(meta.GetHash() != Hash {});
   REQUIRE(meta.GetCppName() == CppNameOf<Deref<T>>());
   REQUIRE(meta.GetName() == NameOf<Deref<T>>());
   REQUIRE(meta.GetInfo() == InfoOf<Deref<T>>());
   REQUIRE(meta.GetFiles() == FilesOf<Deref<T>>());
   REQUIRE(meta.GetSuffix() == SuffixOf<Deref<T>>());
   REQUIRE(meta.GetVersionMajor() == VersionOf<Deref<T>>().Major);
   REQUIRE(meta.GetVersionMinor() == VersionOf<Deref<T>>().Minor);

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      REQUIRE(meta.GetID() != 0);
      REQUIRE(meta.GetBoundaries().empty());
   #endif

   if constexpr (CT::Complete<Decay<T>>)
      REQUIRE(meta.GetOrigin() == MetaDataOf<Decay<T>>());
   else
      REQUIRE(meta.GetOrigin() == nullptr);
   
   if constexpr (CT::Complete<Deptr<Deref<T>>>)
      REQUIRE(meta.GetDeptr() == MetaDataOf<Deptr<Deref<T>>>());
   else
      REQUIRE(meta.GetDeptr() == nullptr);
   
   REQUIRE(meta.GetDecvqAll() == MetaDataOf<DecvqAll<Deref<T>>>());
   REQUIRE(meta.GetDecvq() == MetaDataOf<Decvq<Deref<T>>>());
   REQUIRE_THROWS(meta.AddPtr() == nullptr);
   REQUIRE_THROWS(meta.AddConst() == nullptr);
   
   REQUIRE(meta.GetSize() == sizeof(Deref<T>));
   REQUIRE(meta.GetAlignment() == alignof(Deref<T>));
   REQUIRE(meta.IsConstant() == CT::Constant<T>);
   if constexpr (CT::Complete<Decay<T>>)
      REQUIRE(meta.IsDeep() == CT::Deep<T>);
   else
      REQUIRE(meta.IsDeep() == false);
   REQUIRE(meta.IsPOD() == CT::POD<Deref<T>>);
   REQUIRE(meta.IsNullable() == CT::Nullable<Deref<T>>);
   REQUIRE(meta.IsAbstract() == CT::Abstract<Deref<T>>);
   REQUIRE(meta.HasGetHashMethod() == false);

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      REQUIRE(meta.GetMinPoolsize() == CT::GetMinPool<Deref<T>>());
      REQUIRE(meta.GetPoolTactic() == CT::GetPoolTactic<Deref<T>>());
   #endif
   
   REQUIRE(meta.GetDefaultConstructor() != nullptr);
   REQUIRE(meta.GetDescribeConstructor()== nullptr);
   REQUIRE(meta.GetReferConstructor()   != nullptr);
   REQUIRE(meta.GetCopyConstructor()    != nullptr);
   REQUIRE(meta.GetDisownConstructor()  != nullptr);
   REQUIRE(meta.GetCloneConstructor()   == nullptr);
   REQUIRE(meta.GetMoveConstructor()    != nullptr);
   REQUIRE(meta.GetAbandonConstructor() != nullptr);
   
   REQUIRE(meta.GetDestructor()    == nullptr);
   REQUIRE(meta.GetComparer()      != nullptr);
   REQUIRE(meta.GetComparerEqual() != nullptr);

   if constexpr (CT::Constant<T>) {
      REQUIRE(meta.GetReferAssigner()   == nullptr);
      REQUIRE(meta.GetCopyAssigner()    == nullptr);
      REQUIRE(meta.GetDisownAssigner()  == nullptr);
      REQUIRE(meta.GetCloneAssigner()   == nullptr);
      REQUIRE(meta.GetMoveAssigner()    == nullptr);
      REQUIRE(meta.GetAbandonAssigner() == nullptr);
   }
   else {
      REQUIRE(meta.GetReferAssigner()   != nullptr);
      REQUIRE(meta.GetCopyAssigner()    != nullptr);
      REQUIRE(meta.GetDisownAssigner()  != nullptr);
      REQUIRE(meta.GetCloneAssigner()   == nullptr);
      REQUIRE(meta.GetMoveAssigner()    != nullptr);
      REQUIRE(meta.GetAbandonAssigner() != nullptr);
   }

   REQUIRE(meta.GetResolver()   == nullptr);
   REQUIRE(meta.GetHasher()     != nullptr);
   REQUIRE(meta.GetReferencer() == nullptr);
   REQUIRE(meta.GetDispatcher() == nullptr);
   REQUIRE(meta.GetConcrete()   == nullptr);
   REQUIRE(meta.GetProducer()   == nullptr);

   REQUIRE(meta.GetMembers().size() == 0);
   REQUIRE(meta.GetVerbs().size() == 0);
   REQUIRE(meta.GetBases().size() == 0);
   REQUIRE(meta.GetMorphismsTo().size() == 0);
   REQUIRE(meta.GetMorphismsFrom().size() == 0);
   REQUIRE(meta.GetNamedValues().size() == 0);
}

///                                                                           
/// Reflecting names                                                          
///                                                                           
SCENARIO("Testing reflection of names", "[rtti]") {
#if LANGULUS_FEATURE(MANAGED_MEMORY)
   {
      const DMeta meta = MetaDataOf<pptr8>();
      REQUIRE(meta);
      REQUIRE(meta.GetCppName() == "Langulus::Fractalloc::PackedPointer<char, 2, 6, 0>");
      REQUIRE(meta.GetName() == "Langulus::Fractalloc::PackedPointer<char, 2, 6, 0>");
   }
   {
      const DMeta meta = MetaDataOf<pptr16>();
      REQUIRE(meta);
      REQUIRE(meta.GetCppName() == "Langulus::Fractalloc::PackedPointer<char, 4, 4, 8>");
      REQUIRE(meta.GetName() == "Langulus::Fractalloc::PackedPointer<char, 4, 4, 8>");
   }
   {
      const DMeta meta = MetaDataOf<pptr32>();
      REQUIRE(meta);
      REQUIRE(meta.GetCppName() == "Langulus::Fractalloc::PackedPointer<char>");
      REQUIRE(meta.GetName() == "Langulus::Fractalloc::PackedPointer<char>");
   }
#endif

   {
      const DMeta meta = MetaDataOf<int>();
      REQUIRE(meta);
      REQUIRE(meta.GetCppName() == "int");
      REQUIRE(meta.GetName() == "Int32");
   }
   {
      const DMeta meta = MetaDataOf<const int>();
      REQUIRE(meta);
      REQUIRE(meta.GetCppName() == "int const");
      REQUIRE(meta.GetName() == "Int32 const");
   }
   {
      const DMeta meta = MetaDataOf<const int*>();
      REQUIRE(meta);
      REQUIRE(meta.GetCppName() == "int const*");
      REQUIRE(meta.GetName() == "Int32 const*");
   }
   {
      const DMeta meta = MetaDataOf<int const>();
      REQUIRE(meta);
      REQUIRE(meta.GetCppName() == "int const");
      REQUIRE(meta.GetName() == "Int32 const");
   }
   {
      const DMeta meta = MetaDataOf<int const*>();
      REQUIRE(meta);
      REQUIRE(meta.GetCppName() == "int const*");
      REQUIRE(meta.GetName() == "Int32 const*");
   }
   {
      const DMeta meta = MetaDataOf<IncompleteType*>();
      REQUIRE(meta);
      #if LANGULUS_COMPILER(GCC)
         REQUIRE(meta.GetCppName() == "{anonymous}::IncompleteType*");
      #else
         REQUIRE(meta.GetCppName() == "(anonymous namespace)::IncompleteType*");
      #endif
      REQUIRE(meta.GetName() == "IncompleteType*");
   }
   {
      const DMeta meta = MetaDataOf<const IncompleteType**>();
      REQUIRE(meta);
      #if LANGULUS_COMPILER(GCC)
         REQUIRE(meta.GetCppName() == "{anonymous}::IncompleteType const**");
      #else
         REQUIRE(meta.GetCppName() == "(anonymous namespace)::IncompleteType const**");
      #endif
      REQUIRE(meta.GetName() == "IncompleteType const**");
   }
   {
      const DMeta meta = MetaDataOf<ImplicitlyReflectedDataWithTraits>();
      REQUIRE(meta);
      #if LANGULUS_COMPILER(GCC)
         REQUIRE(meta.GetCppName() == "{anonymous}::ImplicitlyReflectedDataWithTraits");
      #else
         REQUIRE(meta.GetCppName() == "(anonymous namespace)::ImplicitlyReflectedDataWithTraits");
      #endif
      REQUIRE(meta.GetName() == "MyType");
   }
   {
      const DMeta meta = MetaDataOf<ImplicitlyReflectedDataWithTraits*>();
      REQUIRE(meta);
      #if LANGULUS_COMPILER(GCC)
         REQUIRE(meta.GetCppName() == "{anonymous}::ImplicitlyReflectedDataWithTraits*");
      #else
         REQUIRE(meta.GetCppName() == "(anonymous namespace)::ImplicitlyReflectedDataWithTraits*");
      #endif
      REQUIRE(meta.GetName() == "MyType*");
   }
   {
      const DMeta meta = MetaDataOf<ImplicitlyReflectedDataWithTraits const*>();
      REQUIRE(meta);
      #if LANGULUS_COMPILER(GCC)
         REQUIRE(meta.GetCppName() == "{anonymous}::ImplicitlyReflectedDataWithTraits const*");
      #else
         REQUIRE(meta.GetCppName() == "(anonymous namespace)::ImplicitlyReflectedDataWithTraits const*");
      #endif
      REQUIRE(meta.GetName() == "MyType const*");
   }
   {
      const DMeta meta = MetaDataOf<ImplicitlyReflectedDataWithTraits* const*>();
      REQUIRE(meta);
      #if LANGULUS_COMPILER(GCC)
         REQUIRE(meta.GetCppName() == "{anonymous}::ImplicitlyReflectedDataWithTraits* const*");
      #else
         REQUIRE(meta.GetCppName() == "(anonymous namespace)::ImplicitlyReflectedDataWithTraits* const*");
      #endif
      REQUIRE(meta.GetName() == "MyType* const*");
   }
   {
      const VMeta meta = MetaVerbOf<Verbs::Create>();
      REQUIRE(meta);
      REQUIRE(meta == MetaVerbOf<Verbs::Create*>());
      REQUIRE(meta == MetaVerbOf<Verbs::Create const>());
      REQUIRE(meta == MetaVerbOf<Verbs::Create const&>());
      REQUIRE(meta.GetCppName() == "Langulus::Verbs::Create");
      REQUIRE(meta.GetPositiveName() == "create");
      REQUIRE(meta.GetNegativeName() == "destroy");
      REQUIRE(meta.GetPositiveOperator() == " + ");
      REQUIRE(meta.GetNegativeOperator() == " - ");
   }
   {
      const TMeta meta = MetaTagOf<Tags::Name>();
      REQUIRE(meta);
      REQUIRE(meta == MetaTagOf<Tags::Name*>());
      REQUIRE(meta == MetaTagOf<Tags::Name const>());
      REQUIRE(meta == MetaTagOf<Tags::Name const&>());
      REQUIRE(meta.GetCppName() == "Langulus::Tags::Name");
      REQUIRE(meta.GetName() == "name");
   }
   {
      const CMeta meta = MetaConstOf<Pi::Number>();
      REQUIRE(meta);
      #if LANGULUS_COMPILER(GCC)
         REQUIRE(meta.GetCppName() == "<unnamed>::Pi::Number");
      #else
         REQUIRE(meta.GetCppName() == "(anonymous namespace)::Pi::Number");
      #endif
      REQUIRE(meta.GetName() == "Pi::Number");
   }

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      REQUIRE_THROWS(MetaDataOf<ConflictingName>());
      REQUIRE_THROWS(MetaDataOf<ConflictingName*>());
      REQUIRE_THROWS(MetaDataOf<ConflictingName const*>());

      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByNegOp1>());
      REQUIRE_NOTHROW(MetaVerbOf<Verbs::ConflictingByNegOp2>()); // allowed because tokens differ in capitalization, and int hasn't been associated with a verb yet
      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByNegOp3>());

      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByPosOp1>());
      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByPosOp2>());
      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByPosOp3>());

      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByNegToken1>());
      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByNegToken2>());
      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByNegToken3>());

      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByPosToken1>());
      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByPosToken2>());
      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByPosToken3>());

      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByPosToken1*>());
      REQUIRE_THROWS(MetaVerbOf<Verbs::ConflictingByPosToken1* const>());

      REQUIRE_THROWS(MetaTagOf<Tags::ConflictingName1>());
      REQUIRE_THROWS(MetaTagOf<Tags::ConflictingName2>());
      REQUIRE_THROWS(MetaTagOf<Tags::ConflictingName3>());
      REQUIRE_NOTHROW(MetaTagOf<Tags::ConflictingName4>()); // allowed because tokens differ in capitalization, and Pi::Number hasn't been associated with a tag yet
      REQUIRE_THROWS(MetaTagOf<Tags::ConflictingName3*>());
      REQUIRE_THROWS(MetaTagOf<Tags::ConflictingName3* const>());

      REQUIRE_THROWS(MetaConstOf<Pi::ConflictingNumber>());
   #endif
   
   //REQUIRE_THROWS(MetaDataOf<InvalidName1>());  // shouldn't compile
   //REQUIRE_THROWS(MetaDataOf<InvalidName2>());  // shouldn't compile
   //REQUIRE_THROWS(MetaDataOf<InvalidName3>());  // shouldn't compile
   //REQUIRE_THROWS(MetaDataOf<InvalidName4>());  // shouldn't compile
   //REQUIRE_THROWS(MetaDataOf<InvalidName5>());  // shouldn't compile
   //REQUIRE_THROWS(MetaDataOf<InvalidName6>());  // shouldn't compile

   //REQUIRE_THROWS(MetaDataOf<ReservedName1>()); // shouldn't compile
   //REQUIRE_THROWS(MetaDataOf<ReservedName2>()); // shouldn't compile
   //REQUIRE_THROWS(MetaDataOf<ReservedName3>()); // shouldn't compile
   //REQUIRE_THROWS(MetaDataOf<ReservedName4>()); // shouldn't compile
   //REQUIRE_THROWS(MetaDataOf<ReservedName5>()); // shouldn't compile
   //REQUIRE_THROWS(MetaDataOf<ReservedName6>()); // shouldn't compile
}

///                                                                           
/// Reflecting abstracts                                                      
///                                                                           
namespace
{
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
}

namespace Langulus::CTTI
{
   template<>
   struct Abstract<ForcedAbstractExternally> {};
   
   template<>
   struct Verbs<DMeta> {
      using Type = Langulus::Verbs::Create;
   };
}

TEMPLATE_TEST_CASE("Reflecting abstract types", "[rtti]"
   , PureAbstract
   , ForcedAbstractExternally
   , ForcedAbstractInternally
   , InheritedAbstract1
   , InheritedAbstract2
   , InheritedAbstract2ButPrivate
) {
   using T = TestType;
   const DMeta meta = MetaDataOf<T>();
   REQUIRE(meta != nullptr);
   REQUIRE(meta.IsDeep() == false);
   REQUIRE(meta.IsPOD() == false);        // Abstract types can't be POD      
   REQUIRE(meta.IsNullable()  == false);  // Abstract types can't be nullable 
   REQUIRE(meta.GetConcrete() == nullptr);
   REQUIRE(meta.IsAbstract() == true);
   REQUIRE(meta.GetSize() == sizeof(T));
   REQUIRE(meta.GetAlignment() == alignof(T));
}

TEMPLATE_TEST_CASE("Reflecting non-abstract types", "[rtti]"
   , int
   , ImpureVirtual
   , InheritedAbstract1ButPrivate
   , InheritedAbstractExternally
) {
   using T = TestType;
   const DMeta meta = MetaDataOf<T>();
   REQUIRE(meta != nullptr);
   REQUIRE(meta.IsAbstract() == false);
   REQUIRE(meta.IsPOD() == CT::POD<T>);
   REQUIRE(meta.IsNullable() == CT::Nullable<T>);
   REQUIRE(meta.GetConcrete() == nullptr);
}

///                                                                           
/// Reflecting virtual bases                                                  
///                                                                           
namespace
{
   /// Type that has a virtual base                                           
   struct VirtuallyDerived : virtual ImpureVirtual {
      using CTTI_Bases = Types<ImpureVirtual, int>;
   };

   /// Type that has a private non-virtual base                               
   struct PrivatelyDerived : private ImpureVirtual {
      using CTTI_Bases = Types<ImpureVirtual, int, float>;
   };
}

TEMPLATE_TEST_CASE("Reflecting virtual bases", "[rtti]",
   VirtuallyDerived
) {
   using T = TestType;
   const DMeta meta = MetaDataOf<T>();
   T instance {};
   auto instance_base = dynamic_cast<ImpureVirtual*>(&instance);

   REQUIRE(meta.GetBases().size() == 2);
   
   REQUIRE(DMeta(meta.GetBases()[0].type).Is(MetaDataOf<ImpureVirtual>()));
   REQUIRE(meta.GetBases()[0].binaryCompatible == false);
   REQUIRE(meta.GetBases()[0].getBase(&instance) == instance_base);
   
   REQUIRE(DMeta(meta.GetBases()[1].type).Is(MetaDataOf<int>()));
   REQUIRE(meta.GetBases()[1].binaryCompatible == false);
   REQUIRE(meta.GetBases()[1].getBase == nullptr);
}

TEMPLATE_TEST_CASE("Reflecting non-virtual bases", "[rtti]",
   PrivatelyDerived
) {
   using T = TestType;
   const DMeta meta = MetaDataOf<T>();

   REQUIRE(meta.GetBases().size() == 3);
   
   REQUIRE(DMeta(meta.GetBases()[0].type).Is(MetaDataOf<ImpureVirtual>()));
   REQUIRE(meta.GetBases()[0].binaryCompatible == false);
   REQUIRE(meta.GetBases()[0].getBase == nullptr);
   
   REQUIRE(DMeta(meta.GetBases()[1].type).Is(MetaDataOf<int>()));
   REQUIRE(meta.GetBases()[1].binaryCompatible == false);
   REQUIRE(meta.GetBases()[1].getBase == nullptr);
   
   REQUIRE(DMeta(meta.GetBases()[2].type).Is(MetaDataOf<float>()));
   REQUIRE(meta.GetBases()[2].binaryCompatible == false);
   REQUIRE(meta.GetBases()[2].getBase == nullptr);
}

///                                                                           
/// Reflecting a complex type                                                 
///                                                                           
SCENARIO("A type reflected with all traits", "[rtti]") {
   ImplicitlyReflectedDataWithTraits instance;
   auto ptrtobase = static_cast<ImplicitlyReflectedData*>(&instance);
   const DMeta meta = MetaDataOf<ImplicitlyReflectedDataWithTraits>();

   REQUIRE(meta != nullptr);
   REQUIRE(meta.GetName() == "MyType");
   REQUIRE(meta.GetInfo() == "Info about MyType");
   REQUIRE(meta.GetFiles() == "txt, pdf");
   REQUIRE(meta.GetVersionMajor() == 2);
   REQUIRE(meta.GetVersionMinor() == 1);
   REQUIRE(meta.IsDeep() == true);
   REQUIRE(meta.IsPOD() == false);       // not POD due to being abstract     
   REQUIRE(meta.IsNullable() == false);  // not nullable due to being abstract
   REQUIRE(meta.IsAbstract() == true);

   REQUIRE(meta.GetMinAllocation() == 1024_pot);
   for (size_t bit = 0; bit < Bitness; ++bit) {
      REQUIRE(meta.GetAllocationTable()[bit] == (bit < 10 ? 8 : ((size_t {1} << bit) / size_t {128})));
   }

   REQUIRE(meta.GetConcrete().Is(MetaDataOf<ImplicitlyReflectedData>()));
   IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetPoolTactic() == PoolTactic::Type)); // Not by size despite reflected pool tactic, because alignment requirements aren't met
   IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetMinPoolsize() == MinimalPoolSize));
   REQUIRE(meta.GetSize() == sizeof(ImplicitlyReflectedDataWithTraits));
   REQUIRE(meta.GetAlignment() == alignof(ImplicitlyReflectedDataWithTraits));
   REQUIRE(meta.GetOrigin() == meta);
   REQUIRE(meta.IsConstant() == false);
   REQUIRE(meta.GetDeptr() == nullptr);
   REQUIRE(meta.GetDecvqAll() == MetaDataOf<ImplicitlyReflectedDataWithTraits>());

   REQUIRE(meta.GetBases().size() == 1);
   REQUIRE(DMeta(meta.GetBases()[0].type).Is(MetaDataOf<ImplicitlyReflectedData>()));
   REQUIRE(meta.GetBases()[0].binaryCompatible == false);
   REQUIRE(meta.GetBases()[0].getBase(&instance) == ptrtobase);

   REQUIRE(meta.GetVerbs().size() == 1);
   auto ability = meta.GetVerbs().begin();
   REQUIRE(VMeta(ability->first) == MetaVerbOf<Verbs::Create>());
   REQUIRE(ability->second != nullptr);

   REQUIRE(meta.GetMembers().size() == 4);
   
   REQUIRE(meta.GetMembers()[0].extent == 1);
   REQUIRE(meta.GetMembers()[0].member(&instance) == &instance.member);
   REQUIRE(meta.GetMembers()[0].tags.empty());
   REQUIRE(meta.GetMembers()[0].name == "member");
   REQUIRE(DMeta(meta.GetMembers()[0].type()).Is(MetaDataOf<int>()));

   REQUIRE(meta.GetMembers()[1].extent == 1);
   REQUIRE(meta.GetMembers()[1].member(&instance) == &instance.anotherMember);
   REQUIRE(meta.GetMembers()[1].tags.size() == 1);
   REQUIRE(meta.GetMembers()[1].tags.contains(RTTI::DefinitionTag::Reflect<Tags::Name>()));
   REQUIRE(meta.GetMembers()[1].name == "anotherMember");
   REQUIRE(DMeta(meta.GetMembers()[1].type()).Is(MetaDataOf<bool>()));

   REQUIRE(meta.GetMembers()[2].extent == 12);
   REQUIRE(meta.GetMembers()[2].member(&instance) == instance.anotherMemberArray);
   REQUIRE(meta.GetMembers()[2].tags.empty());
   REQUIRE(meta.GetMembers()[2].name == "anotherMemberArray");
   REQUIRE(DMeta(meta.GetMembers()[2].type()).Is(MetaDataOf<int>()));

   REQUIRE(meta.GetMembers()[3].extent == 1);
   REQUIRE(meta.GetMembers()[3].member(&instance) == &instance.sparseMember);
   REQUIRE(meta.GetMembers()[3].tags.empty());
   REQUIRE(meta.GetMembers()[3].name == "sparseMember");
   REQUIRE(DMeta(meta.GetMembers()[3].type()).Is(MetaDataOf<int>()));

   REQUIRE(meta.GetNamedValues().size() == 0);

   const auto intmeta = RTTI::DefinitionData::Reflect<int>();
   REQUIRE(meta.GetMorphismsTo().size() == 1);
   REQUIRE(meta.GetMorphismsTo().at(intmeta).convert != nullptr);

   const auto pimeta = RTTI::DefinitionData::Reflect<Pi>();
   REQUIRE(meta.GetMorphismsFrom().size() == 1);
   REQUIRE(meta.GetMorphismsFrom().at(pimeta).convert != nullptr);

   const auto impmeta = RTTI::DefinitionData::Reflect<ImplicitlyReflectedDataWithTraits>();
   REQUIRE(DMeta(pimeta).GetMorphismsTo().at(impmeta).convert == meta.GetMorphismsFrom().at(pimeta).convert);

   int converted = 1;
   meta.GetMorphismsTo().at(intmeta).convert(&instance, &converted);
   REQUIRE(converted == 664);

   Pi source;
   ImplicitlyReflectedDataWithTraits convertedFromPi1;
   meta.GetMorphismsFrom().at(pimeta).convert(&source, &convertedFromPi1);
   REQUIRE(convertedFromPi1.member == 314);
}

///                                                                           
/// Reflecting verbs                                                          
///                                                                           
SCENARIO("Reflecting a verb", "[rtti]") {
   {
      const VMeta vmeta = {};
      REQUIRE_FALSE(vmeta);
      REQUIRE(vmeta.GetPositiveName() == RTTI::DefinitionVerb::InvalidName);
      REQUIRE(vmeta.GetNegativeName() == RTTI::DefinitionVerb::InvalidName);
      REQUIRE(vmeta.GetInfo() == "");
      REQUIRE(vmeta.GetVersionMajor() == 0);
      REQUIRE(vmeta.GetVersionMinor() == 0);
      REQUIRE(vmeta.GetPositiveOperator() == "");
      REQUIRE(vmeta.GetNegativeOperator() == "");
      REQUIRE(vmeta.GetPrecedence() == 0);
   }
   {
      const auto vdef = RTTI::DefinitionVerb::Reflect<Verbs::Create>();
      const VMeta vmeta = MetaVerbOf<Verbs::Create>();
      REQUIRE(vmeta != nullptr);
      REQUIRE(vmeta.GetPositiveName() == "create");
      REQUIRE(vmeta.GetNegativeName() == "destroy");
      REQUIRE(vmeta.GetInfo().starts_with("Used for allocating new elements."));
      REQUIRE(vmeta.GetVersionMajor() == 6);
      REQUIRE(vmeta.GetVersionMinor() == 10);
      REQUIRE(vmeta.GetPositiveOperator() == " + ");
      REQUIRE(vmeta.GetNegativeOperator() == " - ");
      REQUIRE(vmeta.GetPrecedence() == 5);

      Flow::Verb verb;
      REQUIRE(vmeta.GetContextless()(verb));

      const DMeta dmeta = MetaDataOf<DMeta>();
      REQUIRE(dmeta.GetVerbs().at(vdef)(const_cast<DMeta*>(&dmeta), verb));

      const DMeta dmeta_const = MetaDataOf<const DMeta>();
      REQUIRE(dmeta_const.GetVerbs().at(vdef)(const_cast<DMeta*>(&dmeta_const), verb));
   }
}

///                                                                           
/// Reflecting tags                                                           
///                                                                           
SCENARIO("Reflecting a tag", "[rtti]") {
   {
      const TMeta meta = {};

      REQUIRE_FALSE(meta);
      REQUIRE(meta.GetName() == RTTI::DefinitionTag::InvalidName);
      REQUIRE(meta.GetInfo() == "");
      REQUIRE(meta.GetVersionMajor() == 0);
      REQUIRE(meta.GetVersionMinor() == 0);
   }
   {
      const TMeta meta = MetaTagOf<Tags::Name>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta.GetName() == "name");
      REQUIRE(meta.GetInfo() == "Used for tagging names");
      REQUIRE(meta.GetVersionMajor() == 7);
      REQUIRE(meta.GetVersionMinor() == 10);
   }
}

///                                                                           
/// Reflecting values                                                         
///                                                                           
SCENARIO("Reflecting a value", "[rtti]") {
   {
      const CMeta meta = {};
      REQUIRE_FALSE(meta);
      REQUIRE(meta.GetCppName() == "");
      REQUIRE(meta.GetName() == RTTI::DefinitionConst::InvalidName);
      REQUIRE(meta.GetVersionMajor() == 0);
      REQUIRE(meta.GetVersionMinor() == 0);
   }
   {
      const CMeta meta = MetaConstOf<Pi::Number>();
      REQUIRE(meta);
      #if LANGULUS_COMPILER(GCC)
         REQUIRE(meta.GetCppName() == "<unnamed>::Pi::Number");
      #else
         REQUIRE(meta.GetCppName() == "(anonymous namespace)::Pi::Number");
      #endif
      REQUIRE(meta.GetName() == "Pi::Number");
      REQUIRE(meta.GetVersionMajor() == 1);
      REQUIRE(meta.GetVersionMinor() == 0);
   }
}

///                                                                           
/// Reflecting functions                                                      
///                                                                           
TEMPLATE_TEST_CASE("A reflected function signature", "[rtti]",
   //decltype(FunctionForTesting), // shouldn't compile
   void(*)(void*)
) {
   using Signature = TestType;

   static_assert(    ::std::is_function_v<Deptr<Signature>>);
   static_assert(    CT::Sparse<Signature>);
   static_assert(not CT::Decayed<Signature>);
   static_assert(    CT::Complete<Signature>);

   const DMeta meta = MetaDataOf<Signature>();

   REQUIRE(meta != nullptr);
   REQUIRE(meta.GetName() == "<void(void*)>*");
   REQUIRE(meta.IsSparse());
}
