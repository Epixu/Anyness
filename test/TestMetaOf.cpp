///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/MetaOf.hpp>
#include <Langulus/Values.hpp>
#include <Langulus/CT/Members.hpp>

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
      using CTTI_Info = Yes<"Used for tagging names">;

      Token name;
   };

   template<class T>
   struct TName {
      using CTTI_DefineTag = Yes<"Name">;
      T name;
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
      using CTTI_DefineVerb = VerbToken<"create", "destroy", 5.f>;
      using CTTI_DefineVerbOperator = VerbToken<" + ", " - ">;
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
            return context.Create(v);
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
}

namespace
{
   class IncompleteType;
   
   enum class Pi {
      Number = 314
   };
   
   struct NotReflectable {
      using CTTI_ReflectAs = void;
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
      Tags::TName<bool> anotherMember {};
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
      using CTTI_Pooled    = PooledBySize<250>;
      using CTTI_Concrete  = ImplicitlyReflectedData;
      using CTTI_Abstract  = Yes<>;
      using CTTI_Bases     = ImplicitlyReflectedData;
      using CTTI_Verbs     = Verbs::Create;
      using CTTI_MapsTo    = int;
      using CTTI_MapsFrom  = Pi;
      using CTTI_Values    = No<>;

      using Self = ImplicitlyReflectedDataWithTraits;
      using CTTI_Members   = Members<
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
      using CTTI_MapsFrom  = Pi;
      using CTTI_Values    = No<>;
   };
   
   struct CheckingWhatGetsInherited : ImplicitlyReflectedDataWithTraits {
      using CTTI_Named = Yes<"CheckingWhatGetsInherited">;

      using ImplicitlyReflectedDataWithTraits::ImplicitlyReflectedDataWithTraits;
   };
   
   void FunctionForTesting(void*) {
      Logger::Verbose("Executed FunctionForTesting");
   }
}

TEMPLATE_TEST_CASE("Testing reflection of incomplete types", "[rtti]",
   //void, // shouldn't compile
   //nullptr_t, // shouldn't compile
   //IncompleteType, // shouldn't compile
   //NotReflectable, // shouldn't compile
   IncompleteType*,
   IncompleteType**,
   const IncompleteType**,
   const IncompleteType* const*&,
   const IncompleteType* const* const&,
   const IncompleteType* const* const&&
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

   IF_LANGULUS_MANAGED_REFLECTION(REQUIRE(meta.GetID() != 0));
   IF_LANGULUS_MANAGED_REFLECTION(REQUIRE(meta.GetBoundaries().empty()));

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
   REQUIRE(meta.AddPtr() == nullptr);
   REQUIRE(meta.AddConst() == nullptr);
   
   REQUIRE(meta.GetSize() == sizeof(Deref<T>));
   REQUIRE(meta.GetAlignment() == alignof(Deref<T>));
   REQUIRE(meta.IsConstant() == CT::Constant<T>);      
   REQUIRE(meta.IsDeep() == CT::Deep<Deref<T>>);
   REQUIRE(meta.IsPOD() == CT::POD<Deref<T>>);
   REQUIRE(meta.IsNullable() == CT::Nullable<Deref<T>>);
   REQUIRE(meta.IsAbstract() == CT::Abstract<Deref<T>>);
   REQUIRE(meta.GetMinPoolsize() == (Roof2(sizeof(T) * 256 <= LANGULUS_MIN_POOL ? LANGULUS_MIN_POOL : sizeof(T) * 256)));

   IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetPoolTactic() == CT::GetPoolTactic<Deref<T>>()));
   IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetPoolchain() == nullptr));
   REQUIRE(meta.HasGetHashMethod() == false);

   REQUIRE(meta.GetDefaultConstructor() != nullptr);
   REQUIRE(meta.GetDescribeConstructor() == nullptr);
   REQUIRE(meta.GetReferConstructor() != nullptr);
   REQUIRE(meta.GetCopyConstructor() != nullptr);
   REQUIRE(meta.GetDisownConstructor() != nullptr);
   REQUIRE(meta.GetCloneConstructor() == nullptr);
   REQUIRE(meta.GetMoveConstructor() != nullptr);
   REQUIRE(meta.GetAbandonConstructor() != nullptr);
   
   REQUIRE(meta.GetDestructor() == nullptr);
   REQUIRE(meta.GetComparer() != nullptr);

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

   REQUIRE(meta.GetResolver() == nullptr);
   REQUIRE(meta.GetHasher() != nullptr);
   REQUIRE(meta.GetReferencer() == nullptr);
   REQUIRE(meta.GetDispatcher() == nullptr);
   REQUIRE(meta.GetConcrete() == nullptr);
   REQUIRE(meta.GetProducer() == nullptr);

   REQUIRE(meta.GetMembers().size() == 0);
   REQUIRE(meta.GetAbilities().size() == 0);
   REQUIRE(meta.GetBases().size() == 0);
   REQUIRE(meta.GetMorphismsTo().size() == 0);
   REQUIRE(meta.GetMorphismsFrom().size() == 0);
   REQUIRE(meta.GetNamedValues().size() == 0);
}

SCENARIO("A type reflected with all traits", "[rtti]") {
   ImplicitlyReflectedDataWithTraits instance;
   auto ptrtobase = static_cast<ImplicitlyReflectedData*>(&instance);
   const size_t baseoffset = reinterpret_cast<char*>(ptrtobase) - reinterpret_cast<char*>(&instance);
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
   IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetPoolTactic() == PoolTactic::Size));
   REQUIRE(meta.GetConcrete().Is(MetaDataOf<ImplicitlyReflectedData>()));
   IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetMinPoolsize() == MinimalPoolSize));
   REQUIRE(meta.IsAbstract() == true);
   REQUIRE(meta.GetSize() == sizeof(ImplicitlyReflectedDataWithTraits));
   REQUIRE(meta.GetAlignment() == alignof(ImplicitlyReflectedDataWithTraits));
   REQUIRE(meta.GetOrigin() == meta);
   REQUIRE(meta.IsConstant() == false);
   REQUIRE(meta.GetDeptr() == nullptr);
   REQUIRE(meta.GetDecvqAll() == MetaDataOf<ImplicitlyReflectedDataWithTraits>());

   REQUIRE(meta.GetBases().size() == 1);
   REQUIRE(DMeta(meta.GetBases()[0].type).Is(MetaDataOf<ConvertibleData>()));
   REQUIRE(meta.GetBases()[0].imposed == false);
   REQUIRE(meta.GetBases()[0].binaryCompatible == false);
   REQUIRE(meta.GetBases()[0].count == 1);
   REQUIRE(baseoffset >= 0);
   REQUIRE(meta.GetBases()[0].offset == baseoffset);

   REQUIRE(meta.GetAbilities().size() == 1);
   auto ability = meta.GetAbilities().begin();
   REQUIRE(VMeta(ability->first) == MetaVerbOf<Verbs::Create>());
   REQUIRE(ability->second != nullptr);

   REQUIRE(meta.GetMembers().size() == 3);
   REQUIRE(meta.GetMembers()[0].extent == 1);
   REQUIRE(meta.GetMembers()[0].member(&instance) == &instance.anotherMember);
   REQUIRE(TMeta(meta.GetMembers()[0].getTag(0)) == MetaTagOf<Tags::Name>());
   REQUIRE(meta.GetMembers()[0].getTag(1) == nullptr);
   REQUIRE(DMeta(meta.GetMembers()[0].type()).Is(MetaDataOf<bool>()));

   REQUIRE(meta.GetMembers()[1].extent == 12);
   REQUIRE(meta.GetMembers()[1].member(&instance) == instance.anotherMemberArray);
   REQUIRE(meta.GetMembers()[1].getTag(0) == nullptr);
   REQUIRE(DMeta(meta.GetMembers()[1].type()).Is(MetaDataOf<int>()));

   REQUIRE(meta.GetMembers()[2].extent == 1);
   REQUIRE(meta.GetMembers()[2].member(&instance) == &instance.sparseMember);
   REQUIRE(meta.GetMembers()[2].getTag(0) == nullptr);
   REQUIRE(DMeta(meta.GetMembers()[2].type()).Is(MetaDataOf<int>()));

   REQUIRE(meta.GetNamedValues().size() == 0);

   const auto intmeta = RTTI::DefinitionData::Reflect<int>();
   REQUIRE(meta.GetMorphismsTo().size() == 1);
   REQUIRE(meta.GetMorphismsTo().at(intmeta) != nullptr);

   const auto pimeta = RTTI::DefinitionData::Reflect<Pi>();
   REQUIRE(meta.GetMorphismsFrom().size() == 1);
   REQUIRE(meta.GetMorphismsFrom().at(pimeta) != nullptr);

   const auto impmeta = RTTI::DefinitionData::Reflect<ImplicitlyReflectedDataWithTraits>();
   REQUIRE(DMeta(pimeta).GetMorphismsTo().at(impmeta) == meta.GetMorphismsFrom().at(pimeta));

   int converted = 1;
   meta.GetMorphismsTo().at(intmeta)(&instance, &converted);
   REQUIRE(converted == 664);

   Pi source;
   ImplicitlyReflectedDataWithTraits convertedFromPi1;
   meta.GetMorphismsFrom().at(pimeta)(&source, &convertedFromPi1);
   REQUIRE(convertedFromPi1.member == 314);
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
      virtual auto PureVirtualMethod() -> size_t = 0;
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
   struct Abstract<ForcedAbstractExternally> {
      static constexpr bool Enabled = true;
   };
}


TEMPLATE_TEST_CASE("Reflecting abstract types", "[rtti]",
   PureAbstract,
   ForcedAbstractExternally,
   ForcedAbstractInternally,
   InheritedAbstract1,
   InheritedAbstract2,
   InheritedAbstract2ButPrivate
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

TEMPLATE_TEST_CASE("Reflecting non-abstract types", "[rtti]",
   //nullptr_t, // shouldn't compile
   int,
   ImpureVirtual,
   InheritedAbstract1ButPrivate,
   InheritedAbstractExternally
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

   };

   /// Type that has a private non-virtual base                               
   struct PrivatelyDerived : private ImpureVirtual {

   };
}

TEMPLATE_TEST_CASE("Reflecting virtual bases", "[rtti]",
   VirtuallyDerived
) {
   using T = TestType;
   const DMeta meta = MetaDataOf<T>();
   //TODO
}


///                                                                           
/// Reflecting verbs                                                          
///                                                                           
SCENARIO("Reflecting a verb", "[rtti]") {
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
   REQUIRE(dmeta.GetAbilities().at(vdef)(const_cast<DMeta*>(&dmeta), verb));

   const DMeta dmeta_const = MetaDataOf<const DMeta>();
   REQUIRE(dmeta_const.GetAbilities().at(vdef)(const_cast<DMeta*>(&dmeta_const), verb));
}


///                                                                           
/// Reflecting tags                                                           
///                                                                           
SCENARIO("Reflecting a tag", "[rtti]") {
   const TMeta meta = MetaTagOf<Tags::Name>();

   REQUIRE(meta != nullptr);
   REQUIRE(meta.GetName() == "name");
   REQUIRE(meta.GetInfo() == "Used for tagging names");
   REQUIRE(meta.GetVersionMajor() == 7);
   REQUIRE(meta.GetVersionMinor() == 10);
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
