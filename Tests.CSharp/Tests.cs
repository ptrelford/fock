using System;
using System.Collections.Generic;
using System.ComponentModel;
using Microsoft.FSharp.Control;
using NUnit.Framework;
using Fock.Linq;

[TestFixture]
public class Tests
{
    [Test]
    public void TestFunc()
    {
        var stub = 
            new Stub<IList<int>>()
                .SetupFunc(x => x.Contains(It.IsAny<int>())).Returns(true)
                .Create();
        Assert.IsTrue(stub.Contains(1));
    }

    [Test]
    public void TestAction()
    {
        var stub =
            new Stub<IList<int>>()
                .SetupAction(x => x.Clear()).Raises<System.ApplicationException>()
                .Create();
        Assert.Throws<ApplicationException>(() =>
            stub.Clear()
        );
    }

    [Test]
    public void TestPropertyGet()
    {
        var stub =
            new Stub<IList<int>>()
                .SetupPropertyGet(x => x.Count).Returns(1)
                .Create();
        Assert.AreEqual(1, stub.Count);
    }

    [Test]
    public void TestPropertySet()
    {
        var stub =
            new Stub<IList<int>>()
                .SetupPropertySet(x => x[1] ).Raises<System.ApplicationException>()
                .Create();
        Assert.Throws<ApplicationException>(() =>
            stub[1] = 1
        );
    }

    [Test]
    public void TestEvent()
    {
        var fe = new FSharpEvent<PropertyChangedEventHandler, PropertyChangedEventArgs>();
        var stub =
            new Stub<INotifyPropertyChanged>()
                .SetupEvent("PropertyChanged").Publishes(fe.Publish)
                .Create();
        var triggered = false;
        var name = "Name";
        stub.PropertyChanged += (s, e) => triggered = (e.PropertyName == name);
        fe.Trigger(this, new PropertyChangedEventArgs(name));
        Assert.IsTrue(triggered);
    }
}
